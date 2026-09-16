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
    // Trait dictionaries are resolved here too once traits are ported; a domain
    // cannot be a dictionary type before then.
    private static (Term, Value) InsertImplicitArgs(Context ctx, Term term, Value type)
    {
        while (ctx.Force(type) is Value.VPi { Explicitness: Explicitness.Implicit } pi)
        {
            var arg = FreshMeta(ctx);
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
                throw new NotImplementedException("not ported yet: applying a value of unknown function type");
            default:
                throw new FunException("applying non-function");
        }
    }
}
