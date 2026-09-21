using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Elaborator
{
    /// <summary>
    /// <c>v.m</c> on a record of a struct type: a method of that type applied to the
    /// value. The method's type is <c>Self -&gt; …</c>; the value's type unifies with
    /// its <c>Self</c>, the rest is the type of <c>v.m</c>, and the row the method
    /// declares is performed by the call. A method with no parameters
    /// (<c>method m()</c>) is a function of <c>()</c>, so <c>v.m</c> is a function of
    /// unit and <c>v.m()</c> runs it.
    /// </summary>
    private static (Term, Value) MethodCall(Context ctx, Term of, Value ofType, string name, Value methodType)
    {
        if (ctx.Force(methodType) is not Value.VPi pi) throw new FunException("applying non-function");
        ctx.Unify(ofType, pi.Domain);
        var ofValue = ctx.Eval(of);
        var row = Nbe.EvalRowClosure(ctx.Metas, pi.Row, ofValue);
        var result = ctx.Force(Nbe.ApplyClosure(ctx.Metas, pi.Codomain, ofValue));

        if (result is Value.VPi rest)
        {
            Emit(ctx, row.Effects, row.Tails);
            return (new Term.Dot(of, name), rest);
        }

        // The Lam's own binder shifts every term it closes over out by one.
        var arrowRow = new RowClosure(ctx.Environment, new RowTerm(
            [.. row.Effects.Select(e => ctx.Quote(e).Shift(1))],
            [.. row.Tails.Select(t => ctx.Quote(t).Shift(1))]));
        return (new Term.Lam(new Term.Dot(of.Shift(1), name)),
                new Value.VPi(Explicitness.Explicit, new Value.VAtomTy(AtomTy.Unit),
                    new Closure(ctx.Environment, ctx.Quote(result).Shift(1)))
                {
                    Row = arrowRow,
                });
    }
}
