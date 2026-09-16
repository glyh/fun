using Fun.Compiler;
using Fun.Kernel;

namespace Fun.Tests;

public class NbeTests
{
    private static Term Id => new Term.Lam(new Term.Var(0));

    /// <summary>
    /// The evaluator is a loop over a heap stack of frames: a million nested
    /// calls, each waiting on the next, would overflow the CLR's 1 MB stack under
    /// native recursion.
    /// </summary>
    [Fact]
    public void DeepNonTailNestingDoesNotUseTheNativeStack()
    {
        Term term = new Term.Atom(new Atom.I64(42));
        for (var i = 0; i < 1_000_000; i++)
            term = new Term.Ap(Id, Explicitness.Explicit, term);

        var value = Nbe.Eval(new MetaContext(), Environment.Empty, term);

        Assert.Equal(new Value.VAtom(new Atom.I64(42)), value);
    }

    [Fact]
    public void LetPushesItsDefinitionForTheBody()
    {
        // let _ = 1 in let _ = 2 in (Var 1, Var 0)
        var term = new Term.Let(Term.U.Instance, new Term.Atom(new Atom.I64(1)),
            new Term.Let(Term.U.Instance, new Term.Atom(new Atom.I64(2)),
                new Term.Prod([new Term.Var(1), new Term.Var(0)])));

        var value = Assert.IsType<Value.VProd>(Nbe.Eval(new MetaContext(), Environment.Empty, term));

        Assert.Equal<Value>([new Value.VAtom(new Atom.I64(1)), new Value.VAtom(new Atom.I64(2))], value.Items);
    }

    /// <summary>Readback turns levels into indices: `fn(x) { fn(y) { x } }` round-trips.</summary>
    [Fact]
    public void QuoteInvertsEval()
    {
        var term = new Term.Lam(new Term.Lam(new Term.Var(1)));
        var mc = new MetaContext();

        Assert.Equal(term, Nbe.Quote(mc, 0, Nbe.Eval(mc, Environment.Empty, term)));
    }
}
