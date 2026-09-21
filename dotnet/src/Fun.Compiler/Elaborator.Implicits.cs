using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Elaborator
{
    /// <summary>
    /// A fresh meta as created: applied, when evaluated, to every bound entry in
    /// the context, so its solution may depend on exactly those.
    /// </summary>
    private static Term FreshMeta(Context ctx) => new Term.InsertedMeta(ctx.Metas.Fresh(), ctx.EntryKinds);

    /// <summary>
    /// Supplies a meta for every leading implicit parameter of
    /// <paramref name="type"/>, so a use of an implicit function meets the
    /// explicit parameter behind them.
    /// </summary>
    // A dictionary domain is resolved from evidence instead; insertion stops at one
    // not resolvable yet, for the application to resolve after its argument.
    private static (Term, Value) InsertImplicitArgs(Context ctx, Term term, Value type)
    {
        while (ctx.Force(type) is Value.VPi { Explicitness: Explicitness.Implicit } pi)
        {
            Term arg;
            if (ctx.Force(pi.Domain) is Value.VTraitDict dict)
            {
                if (ResolveEvidence(ctx, dict.Decl, dict.Args) is not { } evidence) break;
                arg = evidence.Term;
            }
            else arg = FreshMeta(ctx);
            term = new Term.Ap(term, Explicitness.Implicit, arg);
            type = Nbe.ApplyClosure(ctx.Metas, pi.Codomain, ctx.Eval(arg));
        }
        return (term, type);
    }

    /// <summary><c>f[a]</c>: an implicit argument written out, checked against the implicit domain.</summary>
    private static (Term, Value) InferApImplicit(Context ctx, Syntax.Ap ap)
    {
        var (fn, fnType) = Infer(ctx, ap.Fn);
        switch (ctx.Force(fnType))
        {
            case Value.VPi { Explicitness: Explicitness.Implicit } pi:
            {
                var arg = Check(ctx, ap.Arg, pi.Domain);
                var result = Nbe.ApplyClosure(ctx.Metas, pi.Codomain, ctx.Eval(arg));
                return (new Term.Ap(fn, Explicitness.Implicit, arg), ctx.Force(result));
            }
            case Value.VMeta or Value.VVar or Value.VNeutral:
                return InferApImplicitUnknown(ctx, fn, fnType, ap.Arg);
            default:
                throw new FunException("applying non-function");
        }
    }

    /// <summary>
    /// A value of unknown type applied to a written implicit argument: its type is
    /// taken to be an implicit arrow from a fresh meta to a fresh meta, and unified
    /// with it (elab_apply.ml:158-175) - the implicit analogue of
    /// <see cref="InferApUnknown"/>.
    /// </summary>
    private static (Term, Value) InferApImplicitUnknown(Context ctx, Term fn, Value fnType, Syntax arg)
    {
        var domain = ctx.RawMeta();
        var argTerm = Check(ctx, arg, domain);
        var codomain = new Closure(ctx.Environment, ctx.Bind("_", domain).Quote(ctx.RawMeta()));
        ctx.Unify(fnType, new Value.VPi(Explicitness.Implicit, domain, codomain));
        return (new Term.Ap(fn, Explicitness.Implicit, argTerm),
            Nbe.ApplyClosure(ctx.Metas, codomain, ctx.Eval(argTerm)));
    }

    /// <summary>
    /// A value that is not an implicit lambda, checked against an implicit function
    /// type: the type's implicit parameter is bound first, as checking a lambda binds
    /// it, and the value is checked in that scope, where its own implicit arguments
    /// are inserted against the body (check-against-implicit-type-inserts-first).
    /// A dictionary parameter is bound as evidence.
    /// </summary>
    private static Term CheckUnderImplicit(Context ctx, Syntax stx, Value.VPi pi)
    {
        var (inner, entry) = ctx.BindAnonymous(pi.Domain);
        if (ctx.Force(pi.Domain) is Value.VTraitDict dict)
            inner = inner.AddEvidence(new TraitEvidence(dict.Decl, dict.Args, entry.Level, dict));
        var body = Nbe.ApplyClosure(ctx.Metas, pi.Codomain, new Value.VVar(entry.Level, []));
        return new Term.Lam(Check(inner, stx, body));
    }
}
