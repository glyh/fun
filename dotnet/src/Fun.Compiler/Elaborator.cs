using System.Collections.Immutable;
using Fun.Kernel;

namespace Fun.Compiler;

/// <summary>A name's entry in a scope: the level it was pushed at, and its type.</summary>
public sealed record Entry(int Level, Value Type);

/// <summary>
/// A context: one ordered sequence of entries viewed through columns that must
/// stay the same length, its <see cref="Width"/>. <see cref="Environment"/> is the column
/// the evaluator receives; <see cref="EntryKinds"/> is the mask a meta is
/// abstracted over. An entry's type lives in <see cref="Names"/>, keyed by the
/// resolved name expansion gave its binder.
/// </summary>
public sealed record Context(
    Environment Environment,
    int Width,
    EquatableArray<EntryKind> EntryKinds,
    ImmutableDictionary<string, Entry> Names,
    MetaContext Metas,
    bool PreludeOpen)
{
    /// <summary>Each open entered, by label, with the members it brought in. An open choice locates a name here.</summary>
    public ImmutableDictionary<string, ImmutableDictionary<string, Entry>> Opened { get; init; } =
        ImmutableDictionary<string, ImmutableDictionary<string, Entry>>.Empty;

    /// <summary>The base context's names: where an open choice with no candidate and no binder is located.</summary>
    public ImmutableDictionary<string, Entry> BaseNames { get; init; } = ImmutableDictionary<string, Entry>.Empty;

    public static Context Empty(MetaContext metas, bool preludeOpen) =>
        new(Environment.Empty, 0, [], ImmutableDictionary<string, Entry>.Empty, metas, preludeOpen);

    /// <summary>Pushes a defined entry no name reaches directly: an opened member, located through its open.</summary>
    public (Context, Entry) DefineAnonymous(Value type, Value value) =>
        (this with
        {
            Environment = Environment.Push(value),
            Width = Width + 1,
            EntryKinds = EntryKinds.Insert(0, EntryKind.Defined),
        }, new Entry(Width, type));

    /// <summary>Pushes a bound variable: its value is unknown, so it stands as itself.</summary>
    public Context Bind(string name, Value type) => this with
    {
        Environment = Environment.Push(new Value.VVar(Width, [])),
        Width = Width + 1,
        EntryKinds = EntryKinds.Insert(0, EntryKind.Bound),
        Names = Names.SetItem(name, new Entry(Width, type)),
    };

    /// <summary>Pushes a definition: its value is known.</summary>
    public Context Define(string name, Value type, Value value) => this with
    {
        Environment = Environment.Push(value),
        Width = Width + 1,
        EntryKinds = EntryKinds.Insert(0, EntryKind.Defined),
        Names = Names.SetItem(name, new Entry(Width, type)),
    };

    /// <summary>
    /// The entry a name denotes: a binder by the resolved name expansion gave
    /// it, else the base context. Inside the prelude's open a name the base
    /// lacks may be one of `std`'s members, which are not ported, so it is not
    /// yet known to be unbound.
    /// </summary>
    // Checking the base before `std` gives the prototype's answer (opens first,
    // then the base) only while `std` rebinds no base-context name, which holds
    // today: the base names it spells are members of its `Syntax` module.
    public (int Index, Value Type) Locate(string name)
    {
        if (Names.TryGetValue(name, out var entry)) return At(entry);
        if (PreludeOpen) throw new NotImplementedException($"not ported yet: `{name}` from the prelude");
        throw new FunException($"unbound variable: {name}");
    }

    /// <summary>
    /// An open choice: the first candidate open that has the member, else the
    /// binder it shadows, else the base context.
    /// </summary>
    public (int Index, Value Type) LocateChoice(string name, EquatableArray<string> opens, string? fallback)
    {
        foreach (var label in opens)
            if (Opened.TryGetValue(label, out var members) && members.TryGetValue(name, out var member))
                return At(member);
        if (fallback is not null) return Locate(fallback);
        if (BaseNames.TryGetValue(name, out var based)) return At(based);
        if (PreludeOpen) throw new NotImplementedException($"not ported yet: `{name}` from the prelude");
        throw new FunException($"unbound variable: {name}");
    }

    private (int Index, Value Type) At(Entry entry) => (Nbe.LevelToIndex(Width, entry.Level), entry.Type);

    public Value Eval(Term term) => Nbe.Eval(Metas, Environment, term);
    public Term Quote(Value value) => Nbe.Quote(Metas, Width, value);
    public Value Force(Value value) => Nbe.Force(Metas, value);

    /// <summary>A meta with no spine, standing for a type not written.</summary>
    public Value RawMeta() => new Value.VMeta(Metas.Fresh(), []);

    public void Unify(Value expected, Value inferred)
    {
        try
        {
            Fun.Compiler.Unify.Values(Metas, Width, expected, inferred);
        }
        catch (UnifyException e)
        {
            throw new FunException($"type mismatch: {e.Message}");
        }
    }
}

/// <summary>
/// Bidirectional elaboration of expanded syntax into core terms: <see cref="Infer"/>
/// synthesises a type, <see cref="Check"/> verifies against one.
/// </summary>
public static partial class Elaborator
{
    /// <summary>
    /// The base context: the atom types, as definitions. A program's indices
    /// count these entries, so it runs in this context's environment.
    /// </summary>
    public static Context BaseContext(MetaContext metas, bool preludeOpen)
    {
        var ctx = Context.Empty(metas, preludeOpen);
        foreach (var (name, ty) in new (string, AtomTy)[]
                 {
                     ("I64", AtomTy.I64), ("Unit", AtomTy.Unit), ("Char", AtomTy.Char),
                     ("String", AtomTy.String), ("Scopes", AtomTy.Scopes), ("Absurd", AtomTy.Absurd),
                 })
            ctx = ctx.Define(name, Value.VU.Instance, new Value.VAtomTy(ty));
        ctx = ctx.Define("Type", Value.VU.Instance, Value.VU.Instance);
        return ctx with { BaseNames = ctx.Names };
    }

    /// <summary>
    /// Elaborates a program read as an expression. Such a program has nowhere to
    /// write <c>open (import "std")</c>, so it is elaborated inside that open.
    /// </summary>
    public static Elaborated ElaborateProgram(Syntax program)
    {
        var metas = new MetaContext();
        var ctx = BaseContext(metas, preludeOpen: true);
        var (term, type) = Infer(ctx, program);
        return new Elaborated(term, type, ctx);
    }

    public static (Term, Value) Infer(Context ctx, Syntax stx)
    {
        switch (stx)
        {
            case Syntax.Atom a:
                return (new Term.Atom(a.Value), new Value.VAtomTy(a.Value switch
                {
                    Atom.I64 => AtomTy.I64,
                    Atom.Unit => AtomTy.Unit,
                    Atom.Char => AtomTy.Char,
                    Atom.Str => AtomTy.String,
                    Atom.Scopes => AtomTy.Scopes,
                    _ => throw new InvalidOperationException($"unhandled atom {a.Value.GetType().Name}"),
                }));

            case Syntax.Var v:
            {
                var (index, type) = ctx.Locate(v.Id.Name);
                return (new Term.Var(index), type);
            }

            case Syntax.OpenChoice choice:
            {
                var (index, type) = ctx.LocateChoice(choice.Name.Name, choice.Opens, choice.Fallback);
                return (new Term.Var(index), type);
            }

            case Syntax.Module module:
                return InferModule(ctx, module);

            case Syntax.Open open:
            {
                var (body, of, members) = OpenModule(ctx, open.Of, open.Label);
                var (bodyTerm, bodyType) = Infer(body, open.Body);
                return (new Term.Open(of, members, bodyTerm), bodyType);
            }

            case Syntax.FieldAccess access:
            {
                var (of, ofType) = Infer(ctx, access.Of);
                return InferMember(ctx, of, ofType, access.Field);
            }

            case Syntax.Struct st: return InferStruct(ctx, st);
            case Syntax.RecordConstruct record: return InferRecordConstruct(ctx, record);
            case Syntax.Sig sig: return InferSig(ctx, sig);

            case Syntax.Annotated a:
            {
                var type = TypeValue(ctx, a.Type);
                return (Check(ctx, a.Inner, type), type);
            }

            case Syntax.Lam lam:
                return InferLam(ctx, lam);

            case Syntax.Ap { Explicitness: Explicitness.Explicit } ap:
                return InferAp(ctx, ap);

            case Syntax.Let { Recursive: false } let:
            {
                Term valueTerm;
                Value valueType;
                if (let.Type is { } written)
                {
                    valueType = TypeValue(ctx, written);
                    valueTerm = Check(ctx, let.Value, valueType);
                }
                else
                {
                    (valueTerm, valueType) = Infer(ctx, let.Value);
                }
                // ponytail: no let-generalisation yet; the prototype generalises here.
                var body = ctx.Define(let.Name.Name, valueType, ctx.Eval(valueTerm));
                var (bodyTerm, bodyType) = Infer(body, let.Body);
                return (new Term.Let(ctx.Quote(valueType), valueTerm, bodyTerm), bodyType);
            }

            case Syntax.Arrow { Row: null } arrow:
            {
                var domain = TypeTerm(ctx, arrow.Domain);
                var inner = ctx.Bind(arrow.Name?.Name ?? "_", ctx.Eval(domain));
                var codomain = TypeTerm(inner, arrow.Codomain);
                return (new Term.Pi(arrow.Explicitness, domain, codomain), Value.VU.Instance);
            }

            case Syntax.Prod prod:
            {
                var items = prod.Items.Select(i => Infer(ctx, i)).ToEquatableArray();
                return (new Term.Prod([.. items.Select(i => i.Item1)]),
                        new Value.VProdTy([.. items.Select(i => i.Item2)]));
            }

            case Syntax.Proj proj:
            {
                var (of, ofType) = Infer(ctx, proj.Of);
                if (ctx.Force(ofType) is not Value.VProdTy tuple)
                    throw new FunException("projection of a non-tuple");
                if (proj.Index < 0 || proj.Index >= tuple.Items.Length)
                    throw new FunException("tuple length mismatch");
                return (new Term.Proj(of, proj.Index), ctx.Force(tuple.Items[proj.Index]));
            }

            default:
                throw new NotImplementedException($"not ported yet: elaborating {stx.GetType().Name}");
        }
    }

    public static Term Check(Context ctx, Syntax stx, Value expected)
    {
        expected = ctx.Force(expected);
        switch (stx, expected)
        {
            case (Syntax.Lam lam, Value.VPi pi):
            {
                if (lam.Param.Explicitness != pi.Explicitness)
                    throw new FunException("applying non-function");
                // A written parameter type is an annotation like any other: it
                // must agree with the domain expected. The prototype ignores it
                // here (lambda-check-ignores-written-parameter-type).
                if (lam.Param.Type is { } written)
                    ctx.Unify(pi.Domain, TypeValue(ctx, written));
                var inner = ctx.Bind(lam.Param.Name.Name, pi.Domain);
                var bodyType = Nbe.ApplyClosure(ctx.Metas, pi.Codomain, new Value.VVar(ctx.Width, []));
                return new Term.Lam(Check(inner, lam.Body, bodyType));
            }

            case (Syntax.Prod prod, Value.VProdTy tuple):
                if (prod.Items.Length != tuple.Items.Length) throw new FunException("tuple length mismatch");
                return new Term.Prod([.. prod.Items.Zip(tuple.Items, (i, t) => Check(ctx, i, t))]);

            default:
            {
                var (term, inferred) = Infer(ctx, stx);
                AgreeWithExpected(ctx, expected, inferred, term);
                return term;
            }
        }
    }

    /// <summary>
    /// A module: each binding elaborates in the context the ones before it
    /// built, pushing exactly its slots (I2). Its type lists every binding's
    /// member, private ones included, so widths stay aligned; only public ones
    /// are reachable from outside.
    /// </summary>
    // ponytail: no module stamp slot yet; it arrives with nominals (E11), and
    // both sides read the slot list, so adding it moves no index by hand.
    private static (Term, Value) InferModule(Context ctx, Syntax.Module module)
    {
        var inner = ctx;
        var terms = new List<BindingTerm>();
        var entries = new List<ModuleEntry>();

        foreach (var binding in module.Bindings)
        {
            switch (binding)
            {
                case Binding.Let { Recursive: false } let:
                {
                    var (def, type) = Infer(inner, let.Value);
                    var kind = let.Public ? MemberKind.Public : MemberKind.Private;
                    var term = new BindingTerm.Let(Label(let.Name.Name), kind, def);
                    inner = ExtendFromSlots(inner, term, [(let.Name.Name, type)]);
                    terms.Add(term);
                    entries.Add(new ModuleEntry.Field(term.Name, kind, type));
                    break;
                }

                case Binding.Open open:
                {
                    var (after, of, members) = OpenModule(inner, open.Of, open.Label);
                    inner = after;
                    terms.Add(new BindingTerm.Open(of, members));
                    break;
                }

                default:
                    throw new NotImplementedException($"not ported yet: elaborating the binding {binding.GetType().Name}");
            }
        }

        return (new Term.Module([.. terms]), new Value.VModule([.. entries], Partial: false));
    }

    /// <summary>
    /// Pushes a binding's slots, each with the key it is located by and its type;
    /// its value is the slot's term evaluated in the context so far. The payloads
    /// must match the slot list one for one.
    /// </summary>
    private static Context ExtendFromSlots(Context ctx, BindingTerm binding, EquatableArray<(string Key, Value Type)> payloads)
    {
        var slots = binding.Slots() ?? throw new InvalidOperationException("an open has no slot list");
        if (slots.Length != payloads.Length)
            throw new InvalidOperationException($"{slots.Length} slots against {payloads.Length} payloads");
        for (var i = 0; i < slots.Length; i++)
        {
            var value = slots[i].Source switch
            {
                SlotSource.Def d => ctx.Eval(d.Term),
                var other => throw new InvalidOperationException($"unhandled slot source {other.GetType().Name}"),
            };
            ctx = ctx.Define(payloads[i].Key, payloads[i].Type, value);
        }
        return ctx;
    }

    /// <summary>
    /// Opens a module: one entry per public member of its *type*, in order, each
    /// valued by projecting the module. Recorded under the open's label for the
    /// open choices in its region.
    /// </summary>
    private static (Context, Term, EquatableArray<OpenMember>) OpenModule(Context ctx, Syntax of, string label)
    {
        var (term, type) = Infer(ctx, of);
        if (ctx.Force(type) is not Value.VModule moduleType)
            throw ctx.Force(type) is Value.VMeta or Value.VVar or Value.VNeutral
                ? new NotImplementedException("not ported yet: opening a value of unknown type")
                : new FunException("open of a non-module");

        var value = ctx.Eval(term);
        var members = ImmutableDictionary<string, Entry>.Empty;
        var opened = new List<OpenMember>();
        foreach (var field in moduleType.Entries.OfType<ModuleEntry.Field>().Where(f => f.Kind == MemberKind.Public))
        {
            (ctx, var entry) = ctx.DefineAnonymous(field.Value, Nbe.DotValue(value, field.Name));
            members = members.SetItem(field.Name, entry);
            opened.Add(new OpenMember.Field(field.Name));
        }
        return (ctx with { Opened = ctx.Opened.SetItem(label, members) }, term, [.. opened]);
    }

    /// <summary>The written name a resolved name was minted from: a member's label.</summary>
    private static string Label(string resolved) =>
        resolved.IndexOf('#') is var i and >= 0 ? resolved[..i] : resolved;

    /// <summary>
    /// <c>fn(x) { body }</c> with no expected type. An unwritten parameter type
    /// is a fresh meta; the codomain is the body's type read back under
    /// the parameter.
    /// </summary>
    private static (Term, Value) InferLam(Context ctx, Syntax.Lam lam)
    {
        var domain = lam.Param.Type is { } written ? TypeValue(ctx, written) : ctx.RawMeta();
        var inner = ctx.Bind(lam.Param.Name.Name, domain);
        var (body, bodyType) = Infer(inner, lam.Body);
        var codomain = new Closure(ctx.Environment, inner.Quote(bodyType));
        return (new Term.Lam(body), new Value.VPi(lam.Param.Explicitness, domain, codomain));
    }

    private static (Term, Value) InferAp(Context ctx, Syntax.Ap ap)
    {
        var (fn, fnType) = Infer(ctx, ap.Fn);
        switch (ctx.Force(fnType))
        {
            case Value.VPi { Explicitness: Explicitness.Explicit } pi:
            {
                var arg = Check(ctx, ap.Arg, pi.Domain);
                var result = Nbe.ApplyClosure(ctx.Metas, pi.Codomain, ctx.Eval(arg));
                return (new Term.Ap(fn, Explicitness.Explicit, arg), ctx.Force(result));
            }
            case Value.VPi:
                throw new NotImplementedException("not ported yet: implicit argument insertion");
            case Value.VMeta or Value.VVar or Value.VNeutral:
                throw new NotImplementedException("not ported yet: applying a value of unknown function type");
            default:
                throw new FunException("applying non-function");
        }
    }

    /// <summary>A written type, as a term. Its own type must be a universe.</summary>
    private static Term TypeTerm(Context ctx, Syntax stx) => TypeOfExpr(ctx, stx);

    private static Value TypeValue(Context ctx, Syntax stx) => ctx.Eval(TypeTerm(ctx, stx));
}
