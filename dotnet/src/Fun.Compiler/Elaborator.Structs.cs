using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Elaborator
{
    /// <summary>
    /// A struct: its items in source order. A constructor field is a label, not
    /// an entry -- its type sees the bindings written before it, and is kept as a
    /// value read back at the struct's own width. A binding pushes its slots as
    /// in a module; only public ones are members of the struct's type.
    /// </summary>
    private static (Term, Value) InferStruct(Context ctx, Syntax.Struct st)
    {
        var inner = ctx;
        var conFields = new List<(string Name, Term Type, Value Value)>();
        var bindings = new List<BindingTerm>();
        var members = new List<ModuleEntry>();

        foreach (var binding in st.Bindings)
        {
            switch (binding)
            {
                case Binding.Field field:
                {
                    var type = TypeValue(inner, field.Type);
                    conFields.Add((field.Name, ctx.Quote(type), type));
                    break;
                }

                case Binding.Let { Recursive: false } let:
                {
                    var (def, type) = Infer(inner, let.Value);
                    var kind = let.Public ? MemberKind.Public : MemberKind.Private;
                    var term = new BindingTerm.Let(Label(let.Name.Name), kind, def);
                    inner = ExtendFromSlots(inner, term, [(let.Name.Name, type)]);
                    bindings.Add(term);
                    if (let.Public) members.Add(new ModuleEntry.Field(term.Name, kind, type));
                    break;
                }

                case Binding.Open open:
                {
                    var (after, of, opened) = OpenModule(inner, open.Of, open.Label);
                    inner = after;
                    bindings.Add(new BindingTerm.Open(of, opened));
                    break;
                }

                default:
                    throw new NotImplementedException($"not ported yet: the struct item {binding.GetType().Name}");
            }
        }

        RejectDuplicates(conFields.Select(f => f.Name));
        return (new Term.Struct([.. conFields.Select(f => (f.Name, f.Type))], [.. bindings], Partial: false),
                new Value.VStruct([.. conFields.Select(f => (ModuleEntry)new ModuleEntry.Field(f.Name, MemberKind.Field, f.Value)), .. members], Partial: false));
    }

    /// <summary>
    /// <c>P{x = 1}</c>: every constructor field of <c>P</c> given exactly once, each
    /// checked against its type. The record's type is <c>P</c>'s type.
    /// </summary>
    private static (Term, Value) InferRecordConstruct(Context ctx, Syntax.RecordConstruct record)
    {
        var (type, typeType) = Infer(ctx, record.Type);
        if (ctx.Force(typeType) is not Value.VStruct structType)
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
        var self = new Value.VVar(ctx.Width, []);
        var inner = ctx.Bind("sig#self", Value.VU.Instance);
        var bindings = new List<BindingTerm>();

        foreach (var binding in sig.Bindings)
        {
            if (binding is not Binding.Let let)
                throw new NotImplementedException($"not ported yet: the signature item {binding.GetType().Name}");
            var (typeTerm, typeType) = Infer(inner, let.Value);
            var type = inner.Eval(typeTerm);
            CheckTypeLike(inner, typeType, type);
            var label = Label(let.Name.Name);
            bindings.Add(new BindingTerm.Let(label, MemberKind.Public, inner.Quote(type)));
            inner = inner.Define(let.Name.Name, type, Nbe.DotValue(self, label));
        }

        RejectDuplicates(bindings.Select(b => ((BindingTerm.Let)b).Name));
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
        switch (ctx.Force(ModuleTypeOf(ctx, ofType, of)))
        {
            case Value.VModule module:
                var member = module.PublicMember(name) ?? throw new FunException($"no public member `{name}`");
                return (dot, ctx.Force(member.Value));

            case Value.VStruct st when ctx.Force(ctx.Eval(of)) is Value.VStruct:
                return (dot, ctx.Force(LastMember(st, name, k => k is MemberKind.Public or MemberKind.Method)
                    ?? throw new FunException($"no public member `{name}`")));

            case Value.VStruct st:
                if (st.Entries.OfType<ModuleEntry.Field>().FirstOrDefault(f => f.Name == name && f.Kind == MemberKind.Field) is { } field)
                    return (dot, ctx.Force(field.Value));
                if (LastMember(st, name, k => k == MemberKind.Method) is not null)
                    throw new NotImplementedException("not ported yet: method calls");
                if (!st.Partial) throw new FunException($"no field `{name}`");
                return (dot, RequireField(ctx, ofType, st, name));

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
        Value.VU or Value.VAtomTy or Value.VPi or Value.VProdTy or Value.VSig => true,
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

    private static void RejectDuplicates(IEnumerable<string> names)
    {
        var seen = new HashSet<string>();
        foreach (var name in names)
            if (!seen.Add(name)) throw new FunException($"duplicate field `{name}`");
    }
}
