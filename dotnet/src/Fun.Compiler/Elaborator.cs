using System.Collections.Immutable;
using Fun.Kernel;

namespace Fun.Compiler;

/// <summary>A name's entry in a scope: the level it was pushed at, and its type.</summary>
public sealed record NameEntry(int Level, Value Type);

/// <summary>
/// A scope: one ordered sequence of entries viewed through columns that must
/// stay the same length, <see cref="Level"/>. <see cref="Env"/> is the column
/// the evaluator receives; <see cref="Bds"/> is the mask a metavariable is
/// abstracted over. An entry's type lives in <see cref="Names"/>, keyed by the
/// resolved name expansion gave its binder.
/// </summary>
public sealed record Ctx(
    Env Env,
    int Level,
    EquatableArray<Bd> Bds,
    ImmutableDictionary<string, NameEntry> Names,
    MetaContext Metas,
    bool PreludeOpen)
{
    public static Ctx Empty(MetaContext metas, bool preludeOpen) =>
        new(Env.Empty, 0, [], ImmutableDictionary<string, NameEntry>.Empty, metas, preludeOpen);

    /// <summary>Pushes a bound variable: its value is unknown, so it stands as itself.</summary>
    public Ctx Bind(string name, Value type) => this with
    {
        Env = Env.Push(new Value.VRigid(Level, [])),
        Level = Level + 1,
        Bds = Bds.Insert(0, Bd.Bound),
        Names = Names.SetItem(name, new NameEntry(Level, type)),
    };

    /// <summary>Pushes a definition: its value is known.</summary>
    public Ctx Define(string name, Value type, Value value) => this with
    {
        Env = Env.Push(value),
        Level = Level + 1,
        Bds = Bds.Insert(0, Bd.Defined),
        Names = Names.SetItem(name, new NameEntry(Level, type)),
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
    public (int Index, Value Type) Lookup(string name)
    {
        if (Names.TryGetValue(name, out var entry)) return (Nbe.LevelToIndex(Level, entry.Level), entry.Type);
        if (PreludeOpen) throw new NotImplementedException($"not ported yet: `{name}` from the prelude");
        throw new FunException($"unbound variable: {name}");
    }

    public Value Eval(Term term) => Nbe.Eval(Metas, Env, term);
    public Term Quote(Value value) => Nbe.Quote(Metas, Level, value);
    public Value Force(Value value) => Nbe.Force(Metas, value);

    /// <summary>A metavariable with no spine, standing for a type not written.</summary>
    public Value RawMeta() => new Value.VFlex(Metas.Fresh(), []);

    public void Unify(Value expected, Value inferred)
    {
        try
        {
            Fun.Compiler.Unify.Values(Metas, Level, expected, inferred);
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
public static class Elaborator
{
    /// <summary>
    /// The base context: the atom types, as definitions. A program's indices
    /// count these entries, so it runs in this context's environment.
    /// </summary>
    public static Ctx BaseContext(MetaContext metas, bool preludeOpen)
    {
        var ctx = Ctx.Empty(metas, preludeOpen);
        foreach (var (name, ty) in new (string, AtomTy)[]
                 {
                     ("I64", AtomTy.I64), ("Unit", AtomTy.Unit), ("Char", AtomTy.Char),
                     ("String", AtomTy.String), ("Scopes", AtomTy.Scopes), ("Absurd", AtomTy.Absurd),
                 })
            ctx = ctx.Define(name, Value.VU.Instance, new Value.VAtomTy(ty));
        return ctx.Define("Type", Value.VU.Instance, Value.VU.Instance);
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

    public static (Term, Value) Infer(Ctx ctx, Syntax stx)
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
                var (index, type) = ctx.Lookup(v.Id.Name);
                return (new Term.Var(index), type);
            }

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

    public static Term Check(Ctx ctx, Syntax stx, Value expected)
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
                var bodyType = Nbe.ApplyClosure(ctx.Metas, pi.Codomain, new Value.VRigid(ctx.Level, []));
                return new Term.Lam(Check(inner, lam.Body, bodyType));
            }

            case (Syntax.Prod prod, Value.VProdTy tuple):
                if (prod.Items.Length != tuple.Items.Length) throw new FunException("tuple length mismatch");
                return new Term.Prod([.. prod.Items.Zip(tuple.Items, (i, t) => Check(ctx, i, t))]);

            default:
            {
                var (term, inferred) = Infer(ctx, stx);
                ctx.Unify(expected, inferred);
                return term;
            }
        }
    }

    /// <summary>
    /// <c>fn(x) { body }</c> with no expected type. An unwritten parameter type
    /// is a fresh metavariable; the codomain is the body's type read back under
    /// the parameter.
    /// </summary>
    private static (Term, Value) InferLam(Ctx ctx, Syntax.Lam lam)
    {
        var domain = lam.Param.Type is { } written ? TypeValue(ctx, written) : ctx.RawMeta();
        var inner = ctx.Bind(lam.Param.Name.Name, domain);
        var (body, bodyType) = Infer(inner, lam.Body);
        var codomain = new Closure(ctx.Env, inner.Quote(bodyType));
        return (new Term.Lam(body), new Value.VPi(lam.Param.Explicitness, domain, codomain));
    }

    private static (Term, Value) InferAp(Ctx ctx, Syntax.Ap ap)
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
            case Value.VFlex or Value.VRigid or Value.VNeutral:
                throw new NotImplementedException("not ported yet: applying a value of unknown function type");
            default:
                throw new FunException("applying non-function");
        }
    }

    /// <summary>A written type, as a term. Its own type must be a universe.</summary>
    private static Term TypeTerm(Ctx ctx, Syntax stx) => Check(ctx, stx, Value.VU.Instance);

    private static Value TypeValue(Ctx ctx, Syntax stx) => ctx.Eval(TypeTerm(ctx, stx));
}
