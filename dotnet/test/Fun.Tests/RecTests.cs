using Fun.Compiler;
using Fun.Kernel;

namespace Fun.Tests;

public class RecTests
{
    private static Elaborated Elaborate(string source) => Driver.Elaborate(source, new Dictionary<string, string>());

    /// <summary>
    /// A divergent evaluation while checking is a genuine budget error that names
    /// the fixpoint -- not "not ported", which the conformance runner would also
    /// count as an error for an `error` case.
    /// </summary>
    [Theory]
    [InlineData("{ rec loop : I64 -> Type = fn(n) { loop(n) }; g = fn(y : loop(0)) { 1 }; 2 }")]
    [InlineData("{ rec loop : I64 -> Type = fn(n) { loop(n) }; g = fn(n : I64, y : loop(n)) { 1 }; 2 }")]
    [InlineData("{ rec l1 : I64 -> I64 = fn(n) { l1(n) }; rec l2 : I64 -> I64 = fn(n) { l2(n) }; g = fn(F : I64 -> Type, n : I64, y : F(l1(n))) { (y : F(l2(n))) }; 2 }")]
    public void DivergenceWhileCheckingIsABudgetError(string source)
    {
        var error = Assert.Throws<FunException>(() => Elaborate(source));
        Assert.Contains("evaluation exceeded the budget", error.Message);
    }

    /// <summary>Two calls of one pure fixpoint on the same argument are equal without unfolding it.</summary>
    [Fact]
    public void LazyDeltaComparesCallsWithoutUnfolding() =>
        Elaborate("{ rec loop : I64 -> I64 = fn(n) { loop(n) }; g = fn(F : I64 -> Type, n : I64, y : F(loop(n))) { (y : F(loop(n))) }; 2 }");

    /// <summary>
    /// A fixpoint call is a machine frame, not a native one: a non-tail recursion a
    /// million deep runs where native recursion would overflow the CLR stack.
    /// </summary>
    [Fact]
    public void DeepNonTailRecursionDoesNotUseTheNativeStack()
    {
        // rec down = fn(n) { if n == 0 then 0 else (fn(r) { r })(down(n - 1)) }, with the
        // prelude's `if` and `-` replaced by a countdown over a nested tuple spine:
        // down(k) = (fn(r) { r })(down(inner k)), bottoming out at a leaf.
        const int depth = 1_000_000;
        Term chain = new Term.Atom(new Atom.I64(42));
        for (var i = 0; i < depth; i++) chain = new Term.Prod([chain]);

        // down = fn(p) { match-free: p is either (inner) or a leaf; read it by projection }.
        // The body projects the one element and recurses non-tail through an identity call.
        var mc = new MetaContext();
        var id = new Term.Lam(new Term.Var(0));
        // body under [down, p]: id(down(p.0))
        var body = new Term.Lam(new Term.Ap(id, Explicitness.Explicit,
            new Term.Ap(new Term.Var(1), Explicitness.Explicit, new Term.Proj(new Term.Var(0), 0))));
        var down = new Term.Fix([new FixMember("down", Pure: false, body)], 0);

        // Running unbounded: the recursion ends when projecting a leaf fails, which
        // must be reached from the bottom of a million pending calls.
        var error = Assert.Throws<FunException>(() =>
            mc.Budget.Run(() => Nbe.Eval(mc, Environment.Empty, new Term.Ap(down, Explicitness.Explicit, chain))));
        Assert.Equal("projection of a non-tuple", error.Message);
    }
}
