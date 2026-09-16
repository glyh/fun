using Fun.Compiler;
using Fun.Kernel;

namespace Fun.Tests;

public class UnifyTests
{
    private static Value Pi(Term domain, Term codomain) =>
        Nbe.Eval(new MetaContext(), Environment.Empty, new Term.Pi(Explicitness.Explicit, domain, codomain));

    /// <summary>
    /// <c>?0[x] = (T : Type) -&gt; T -&gt; T</c>: the solution mentions its own
    /// binder, so solving has to lift the renaming under it. The prototype raises
    /// VarNotInSpine here (meta-solution-renaming-not-lifted-under-binders).
    /// </summary>
    [Fact]
    public void SolvesADependentRightHandSideUnderASpine()
    {
        var mc = new MetaContext();
        var meta = mc.Fresh();
        var x = new Value.VVar(5, []);
        var dependent = new Term.Pi(Explicitness.Explicit, Term.U.Instance,
            new Term.Pi(Explicitness.Explicit, new Term.Var(0), new Term.Var(1)));

        Unify.Values(mc, 6, new Value.VMeta(meta, [x]), Nbe.Eval(mc, Environment.Empty, dependent));

        // The solution is `fn(_) { (T : Type) -> T -> T }`: applied to x, it reads back as the type.
        var solved = Nbe.Apply(mc, mc.Solution(meta)!, x);
        Assert.Equal(dependent, Nbe.Quote(mc, 6, solved));
    }

    /// <summary>A spine variable becomes the solution's own parameter.</summary>
    [Fact]
    public void AbstractsOverTheSpine()
    {
        var mc = new MetaContext();
        var meta = mc.Fresh();

        // ?0[#2, #4] = (#4, #2)
        Unify.Values(mc, 5,
            new Value.VMeta(meta, [new Value.VVar(2, []), new Value.VVar(4, [])]),
            new Value.VProdTy([new Value.VVar(4, []), new Value.VVar(2, [])]));

        Assert.Equal(new Term.Lam(new Term.Lam(new Term.ProdTy([new Term.Var(0), new Term.Var(1)]))),
            Nbe.Quote(mc, 0, mc.Solution(meta)!));
    }

    [Fact]
    public void RejectsAVariableTheSpineDoesNotAbstract()
    {
        var mc = new MetaContext();
        var meta = mc.Fresh();

        Assert.Throws<UnifyException>(() =>
            Unify.Values(mc, 5, new Value.VMeta(meta, [new Value.VVar(2, [])]), new Value.VVar(3, [])));
    }

    [Fact]
    public void RejectsANonLinearSpine()
    {
        var mc = new MetaContext();
        var meta = mc.Fresh();
        var x = new Value.VVar(2, []);

        Assert.Throws<UnifyException>(() =>
            Unify.Values(mc, 5, new Value.VMeta(meta, [x, x]), new Value.VAtomTy(AtomTy.I64)));
    }
}
