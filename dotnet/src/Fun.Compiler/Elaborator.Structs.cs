using System.Collections.Immutable;
using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Elaborator
{
    /// <summary>
    /// A struct: its items in source order. A constructor field is a label, not
    /// an entry -- its type sees the bindings written before it, and is kept as a
    /// value read back at the struct's own width. A binding pushes its slots as
    /// in a module; only public ones are members of the struct's type. Inside
    /// every item but a field, <c>Self</c> is the fields written so far; a method
    /// needs every field, so one written before the last field is elaborated
    /// right after it.
    /// </summary>
    private static (Term, Value) InferStruct(Context outer, Syntax.Struct st)
    {
        var ctx = outer;
        var conFields = new List<(string Name, Term Type, Value Value)>();
        var bindings = new List<BindingTerm>();
        var members = new List<ModuleEntry>();
        var deferred = new List<Binding.Method>();
        ImmutableDictionary<string, Value>? methodTypes = null;

        Value.VStruct PartialSelf() =>
            new([.. conFields.Select(f => (ModuleEntry)new ModuleEntry.Field(f.Name, MemberKind.Field, f.Value))], Partial: true);

        // Every method's type, known before any method body, so `self.m` can
        // name a method written later.
        // ponytail: read where the first method is elaborated, so a method's
        // annotation cannot name an item written after that point (as the prototype).
        ImmutableDictionary<string, Value> MethodTypes(Context at) => methodTypes ??=
            st.Bindings.OfType<Binding.Method>().ToImmutableDictionary(m => Label(m.Name.Name), m => MethodType(at, PartialSelf(), m));

        void Elaborate(Binding item)
        {
            if (item is Binding.Field field)
            {
                var type = TypeValue(ctx with { SelfType = outer.SelfType }, field.Type);
                conFields.Add((field.Name, outer.Quote(type), type));
                return;
            }

            ctx = ctx with { SelfType = PartialSelf() };
            switch (item)
            {
                case Binding.Let { Recursive: false } let:
                {
                    var (def, type) = Infer(ctx with { SelfEntry = null, SelfMethods = ImmutableDictionary<string, Value>.Empty }, let.Value);
                    AddMember(let.Name.Name, let.Public ? MemberKind.Public : MemberKind.Private, def, type);
                    break;
                }

                case Binding.Method method:
                {
                    var (def, type) = ElaborateMethod(ctx, PartialSelf(), method, MethodTypes(ctx));
                    AddMember(method.Name.Name, method.Public ? MemberKind.Method : MemberKind.PrivateMethod, def, type);
                    break;
                }

                case Binding.Open open:
                {
                    var (after, of, opened) = OpenModule(ctx, open.Of, open.Label, open.RolesInRegion);
                    ctx = after;
                    bindings.Add(new BindingTerm.Open(of, opened));
                    break;
                }

                case Binding.Impl impl:
                {
                    var (after, term, entry) = ElaborateImplItem(ctx, impl);
                    ctx = after;
                    bindings.Add(term);
                    if (impl.Public) members.Add(entry);
                    break;
                }

                default:
                    throw new FunException($"unsupported struct item: {item.GetType().Name}");
            }
        }

        void AddMember(string key, MemberKind kind, Term def, Value type)
        {
            var term = new BindingTerm.Let(Label(key), kind, def);
            ctx = ExtendFromSlots(ctx, term, [(key, type)]);
            bindings.Add(term);
            if (Nbe.Visible(kind)) members.Add(new ModuleEntry.Field(term.Name, kind, type));
        }

        var lastField = st.Bindings.Select((b, i) => b is Binding.Field ? i : -1).DefaultIfEmpty(-1).Max();
        for (var i = 0; i < st.Bindings.Length; i++)
        {
            if (st.Bindings[i] is Binding.Method early && i < lastField) deferred.Add(early);
            else Elaborate(st.Bindings[i]);
            if (i == lastField) deferred.ForEach(Elaborate);
        }

        RejectDuplicates(conFields.Select(f => f.Name));
        return (new Term.Struct([.. conFields.Select(f => (f.Name, f.Type))], [.. bindings], Partial: false),
                new Value.VStruct([.. conFields.Select(f => (ModuleEntry)new ModuleEntry.Field(f.Name, MemberKind.Field, f.Value)), .. members], Partial: false));
    }

    /// <summary>
    /// A method's type before its body is read: <c>Self -> params -> result</c>,
    /// each parameter and the result at its written type, else a meta the body
    /// solves, and its declared row on the innermost arrow.
    /// </summary>
    private static Value MethodType(Context ctx, Value.VStruct self, Binding.Method method)
    {
        var (selfCtx, _) = (ctx with { SelfType = self }).BindAnonymous(self);
        Value Result(Context at) => method.Body is Syntax.Annotated { Type: var written } ? TypeValue(at, written) : at.RawMeta();
        var (type, row) = Params(selfCtx, method.Params, Result, method.Row);
        return new Value.VPi(Explicitness.Explicit, self, new Closure(ctx.Environment, selfCtx.Quote(type)))
        {
            Row = InnermostRow(ctx, method.Params, row),
        };
    }

    /// <summary>
    /// A method: a function of <c>self</c>, then of its parameters. Inside it
    /// <c>self</c> is the value called on and <c>self.m</c> reaches every method;
    /// its body performs within its declared row, which is pure when none is
    /// written (E3); its type must agree with the one promised before any body was
    /// read, rows included.
    /// </summary>
    private static (Term, Value) ElaborateMethod(Context ctx, Value.VStruct self, Binding.Method method, ImmutableDictionary<string, Value> methodTypes)
    {
        var withSelf = ctx with { SelfType = self };
        var (selfCtx, entry) = withSelf.BindAnonymous(self);
        selfCtx = selfCtx with { SelfEntry = entry, SelfMethods = methodTypes, HandlerScopes = [] };

        var (body, bodyType, row) = MethodBody(selfCtx, method.Params, method.Body, method.Row);
        var type = new Value.VPi(Explicitness.Explicit, self, new Closure(withSelf.Environment, selfCtx.Quote(bodyType)))
        {
            Row = InnermostRow(withSelf, method.Params, row),
        };
        if (methodTypes.TryGetValue(Label(method.Name.Name), out var promised)) ctx.Unify(promised, type);
        return (new Term.Lam(body), type);
    }

    private static (Term, Value, RowTerm) MethodBody(Context ctx, EquatableArray<Param> parameters, Syntax body, EffectRow? rowSyntax)
    {
        if (parameters.IsEmpty)
        {
            var ((term, type), performed) = Collecting(ctx, c => Infer(c, body));
            var row = MethodRow(ctx, rowSyntax);
            CheckEffectSubset(ctx, performed, Nbe.EvalRow(ctx.Metas, ctx.Environment, row), inFunction: true);
            return (term, type, row);
        }
        var param = parameters[0];
        var domain = param.Type is { } written ? TypeValue(ctx, written) : ctx.RawMeta();
        var inner = ctx.Bind(param.Name.Name, domain);
        var rest = parameters.RemoveAt(0);
        var (bodyTerm, bodyType, bodyRow) = MethodBody(inner, rest, body, rowSyntax);
        return (new Term.Lam(bodyTerm), new Value.VPi(param.Explicitness, domain, new Closure(ctx.Environment, inner.Quote(bodyType)))
        {
            Row = InnermostRow(ctx, rest, bodyRow),
        }, bodyRow);
    }

    /// <summary>The function type over <paramref name="parameters"/>, ending in the type <paramref name="result"/> gives, with the row on its innermost arrow.</summary>
    private static (Value Type, RowTerm Row) Params(Context ctx, EquatableArray<Param> parameters, Func<Context, Value> result, EffectRow? rowSyntax)
    {
        if (parameters.IsEmpty) return (result(ctx), MethodRow(ctx, rowSyntax));
        var param = parameters[0];
        var domain = param.Type is { } written ? TypeValue(ctx, written) : ctx.RawMeta();
        var inner = ctx.Bind(param.Name.Name, domain);
        var rest = parameters.RemoveAt(0);
        var (type, row) = Params(inner, rest, result, rowSyntax);
        return (new Value.VPi(param.Explicitness, domain, new Closure(ctx.Environment, inner.Quote(type)))
        {
            Row = InnermostRow(ctx, rest, row),
        }, row);
    }

    /// <summary>
    /// A method's declared row, read with every parameter bound. A result written
    /// <c>~&gt; T</c> infers its row from what the body performs, exactly as a
    /// definition's final <c>~&gt;</c> does (method-cannot-infer-row-with-poly-arrow;
    /// the prototype rejects it).
    /// </summary>
    private static RowTerm MethodRow(Context ctx, EffectRow? row) =>
        ElaborateRow(ctx, row is { Polymorphic: true, Inferred: true } ? row with { Polymorphic = false } : row);

    /// <summary>
    /// The row an arrow of a method carries: the declared row on the innermost one
    /// (the arrow whose remaining parameters are <paramref name="rest"/>, none),
    /// pure on every other. <paramref name="outer"/> is the context outside that arrow.
    /// </summary>
    private static RowClosure InnermostRow(Context outer, EquatableArray<Param> rest, RowTerm row) =>
        rest.IsEmpty && !row.IsPure ? new RowClosure(outer.Environment, row) : RowClosure.Pure;

    /// <summary>
    /// <c>P{x = 1}</c>: every constructor field of <c>P</c> given exactly once, each
    /// checked against its type. The record's type is <c>P</c>'s type. A type former
    /// with implicit parameters (<c>fn[A : Type] { struct { … } }</c>) gets them
    /// inserted, as an application does, and the fields solve them.
    /// </summary>
    private static (Term, Value) InferRecordConstruct(Context ctx, Syntax.RecordConstruct record)
    {
        var (type, typeType) = Infer(ctx, record.Type);
        (type, typeType) = InsertImplicitArgs(ctx, type, typeType);
        // A recursive occurrence is a type of type `Type`: its shape is its unfolding.
        var shape = ctx.Force(typeType) is Value.VU ? Nbe.Unfold(ctx.Metas, ctx.Eval(type)) : Nbe.Unfold(ctx.Metas, typeType);
        if (shape is not Value.VStruct structType)
            throw new FunException("record construction of a non-struct");

        var declared = structType.Entries.OfType<ModuleEntry.Field>().Where(f => f.Kind == MemberKind.Field).ToList();
        RejectDuplicates(record.Fields.Select(f => f.Name));
        foreach (var (name, _) in record.Fields)
            if (declared.All(d => d.Name != name)) throw new FunException($"unknown record field `{name}`");
        foreach (var field in declared)
            if (record.Fields.All(f => f.Name != field.Name)) throw new FunException($"missing record field `{field.Name}`");

        var fields = record.Fields.Select(f => (f.Name, Check(ctx, f.Value, declared.First(d => d.Name == f.Name).Value)));
        return (new Term.RecordConstruct(type, [.. fields]), structType);
    }

    /// <summary>
    /// <c>sig { T : Type; empty : T }</c>: a telescope over the module it
    /// describes. Under a binder standing for that module, each member's type is
    /// elaborated with the earlier members bound to that module's members, so
    /// <c>empty : T</c> reads as <c>empty : self.T</c>.
    /// </summary>
    private static (Term, Value) InferSig(Context ctx, Syntax.Sig sig)
    {
        ctx = ctx.WithoutSelf();
        var self = new Value.VVar(ctx.Width, []);
        var inner = ctx.Bind("sig#self", Value.VU.Instance);
        var bindings = new List<BindingTerm>();

        foreach (var binding in sig.Bindings)
        {
            if (binding is Binding.Impl { Fields: null } required)
            {
                inner = InferSignatureImpl(inner, self, required, bindings);
                continue;
            }
            if (binding is not Binding.Let let)
                throw new FunException($"unsupported signature item: {binding.GetType().Name}");
            var (typeTerm, typeType) = Infer(inner, let.Value);
            var type = inner.Eval(typeTerm);
            CheckTypeLike(inner, typeType, type);
            var label = Label(let.Name.Name);
            bindings.Add(new BindingTerm.Let(label, MemberKind.Public, inner.Quote(type)));
            inner = inner.Define(let.Name.Name, type, Nbe.DotValue(self, label));
        }

        RejectDuplicates(bindings.Select(b => b switch
        {
            BindingTerm.Let l => l.Name,
            BindingTerm.Impl { Name: { } name } => name,
            _ => throw new InvalidOperationException($"unhandled signature binding {b.GetType().Name}"),
        }));
        return (new Term.Sig(new Term.Module([.. bindings], Signature: true)), Value.VU.Instance);
    }

    /// <summary>
    /// <c>e.name</c>. A module's public member; a struct's own public member; a
    /// record's field; a signature's member, instantiated with the module. On a
    /// value of unknown type it asks for any struct with that field.
    /// </summary>
    private static (Term, Value) InferMember(Context ctx, Term of, Value ofType, string name)
    {
        var dot = new Term.Dot(of, name);
        switch (Nbe.Unfold(ctx.Metas, ModuleTypeOf(ctx, ofType, of)))
        {
            case Value.VModule module:
                // A named public impl is a member too: its type is its dictionary type.
                if (module.PublicMember(name) is null
                    && module.Entries.OfType<ModuleEntry.Impl>().LastOrDefault(i => i.Name == name && i.Kind == MemberKind.Public) is { } named)
                    return (dot, ctx.Force(named.DictType));
                var member = module.PublicMember(name) ?? throw new FunException($"no public member `{name}`");
                return (dot, ctx.Force(member.Value));

            case Value.VStruct st when ctx.Force(ctx.Eval(of)) is Value.VStruct:
                return (dot, ctx.Force(LastMember(st, name, k => k is MemberKind.Public or MemberKind.Method)
                    ?? throw new FunException($"no public member `{name}`")));

            case Value.VStruct st:
            {
                if (st.Entries.OfType<ModuleEntry.Field>().FirstOrDefault(f => f.Name == name && f.Kind == MemberKind.Field) is { } field)
                    return (dot, ctx.Force(field.Value));
                var isSelf = of is Term.Var v && ctx.SelfEntry is { } self && v.Index == Nbe.LevelToIndex(ctx.Width, self.Level);
                var methodType = LastMember(st, name, k => k == MemberKind.Method)
                    ?? (isSelf && ctx.SelfMethods.TryGetValue(name, out var selfMethod) ? selfMethod : null);
                if (methodType is not null) return MethodCall(ctx, of, ofType, name, methodType);
                // `self` is exactly the fields written so far: it asks for nothing more.
                if (!st.Partial || isSelf) throw new FunException($"no field `{name}`");
                return (dot, RequireField(ctx, ofType, st, name));
            }

            case Value.VMeta or Value.VVar or Value.VNeutral:
                return (dot, RequireField(ctx, ofType, null, name));

            default:
                throw new FunException($"member access `.{name}` on a value with no members");
        }
    }

    /// <summary>
    /// A field of a value whose type is not yet a known struct: its type becomes
    /// (at least) a partial struct holding the field, at a fresh type.
    /// </summary>
    private static Value RequireField(Context ctx, Value ofType, Value.VStruct? known, string name)
    {
        var result = ctx.RawMeta();
        ctx.Unify(ofType, new Value.VStruct([.. known?.Entries ?? [], new ModuleEntry.Field(name, MemberKind.Field, result)], Partial: true));
        return result;
    }

    private static Value? LastMember(Value.VStruct st, string name, Func<MemberKind, bool> kind) =>
        st.Entries.OfType<ModuleEntry.Field>().LastOrDefault(f => f.Name == name && kind(f.Kind))?.Value;

    /// <summary>A module's type as its member types: a signature, instantiated with the module it describes.</summary>
    private static Value ModuleTypeOf(Context ctx, Value type, Term module) =>
        ctx.Force(type) is Value.VSig sig ? ctx.Force(Nbe.ApplyClosure(ctx.Metas, sig.Body, ctx.Eval(module))) : type;

    /// <summary>
    /// What checking falls back to once a form's type is inferred. Against a
    /// universe any type-like value will do; against a signature, the module's
    /// type is what the signature gives that module.
    /// </summary>
    private static void AgreeWithExpected(Context ctx, Value expected, Value inferred, Term term)
    {
        if (ctx.Force(expected) is Value.VU) CheckTypeLike(ctx, inferred, ctx.Eval(term));
        else ctx.Unify(ModuleTypeOf(ctx, expected, term), inferred);
    }

    /// <summary>
    /// A written type: any form whose value is type-like. A module is never a
    /// type; only a signature is.
    /// </summary>
    private static Term TypeOfExpr(Context ctx, Syntax stx)
    {
        var (term, type) = Infer(ctx, stx);
        var value = ctx.Eval(term);
        if (ctx.Force(value) is Value.VModule { Partial: false })
            throw new FunException("a module is not a type; only a signature is");
        CheckTypeLike(ctx, type, value);
        return term;
    }

    /// <summary>A value of type <paramref name="type"/> is a type when its type is a universe or it is type-like.</summary>
    private static void CheckTypeLike(Context ctx, Value type, Value value)
    {
        if (ctx.Force(type) is not Value.VU && !IsTypeLike(ctx, value)) ctx.Unify(type, Value.VU.Instance);
    }

    private static bool IsTypeLike(Context ctx, Value value) => ctx.Force(value) switch
    {
        Value.VU or Value.VEffectRowTy or Value.VAtomTy or Value.VPi or Value.VProdTy or Value.VNominal
            or Value.VEffect or Value.VTraitDict or Value.VRefTy or Value.VSig or Value.VRecursiveOccurrence => true,
        Value.VModule { Partial: true } m => m.Entries.OfType<ModuleEntry.Field>().All(f => f.Kind switch
        {
            MemberKind.Public => IsTypeLike(ctx, f.Value),
            MemberKind.Private => true,
            _ => false,
        }),
        Value.VStruct st => st.Entries.OfType<ModuleEntry.Field>().All(f =>
            f.Kind is MemberKind.Private or MemberKind.PrivateMethod || IsTypeLike(ctx, f.Value)),
        _ => false,
    };

    private static void RejectDuplicates(IEnumerable<string> names, string what = "field")
    {
        var seen = new HashSet<string>();
        foreach (var name in names)
            if (!seen.Add(name)) throw new FunException($"duplicate {what} `{name}`");
    }
}

public sealed partial record Context
{
    /// <summary>Inside a method: the entry holding the value the method was called on.</summary>
    public Entry? SelfEntry { get; init; }

    /// <summary>Inside a struct: the struct being defined, as the fields written so far.</summary>
    public Value? SelfType { get; init; }

    /// <summary>Inside a method: every method of the struct, by name, at its type.</summary>
    public ImmutableDictionary<string, Value> SelfMethods { get; init; } = ImmutableDictionary<string, Value>.Empty;

    /// <summary>Outside any struct: no <c>self</c>, no <c>Self</c>.</summary>
    public Context WithoutSelf() => this with { SelfEntry = null, SelfType = null, SelfMethods = ImmutableDictionary<string, Value>.Empty };

    /// <summary>Pushes a bound entry no name reaches: <c>self</c>, located through <see cref="SelfEntry"/>.</summary>
    public (Context, Entry) BindAnonymous(Value type) =>
        (this with
        {
            Environment = Environment.Push(new Value.VVar(Width, [])),
            Width = Width + 1,
            EntryKinds = EntryKinds.Insert(0, EntryKind.Bound),
        }, new Entry(Width, type));

    public (Term, Value) LocateSelf() =>
        SelfEntry is { } entry
            ? (new Term.Var(Nbe.LevelToIndex(Width, entry.Level)), entry.Type)
            : throw new FunException("unbound variable: self");
}
