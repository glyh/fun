using Fun.Compiler;
using Fun.Kernel;

namespace Fun.Tests;

public class RefsTests
{
    private static Elaborated Elaborate(string source) => Driver.Elaborate(source, new Dictionary<string, string>());

    private static string ErrorOf(string source) =>
        Assert.Throws<FunException>(() => Elaborate(source)).Message;

    /// <summary>A heap allocated inside a function that neither its domain nor its result mentions is discharged: the function is pure.</summary>
    [Fact]
    public void ALocalHeapIsDischargedAtTheFunctionBoundary()
    {
        var pi = Assert.IsType<Value.VPi>(Elaborate("fn(u : Unit) { r = ref(1); deref(r) }").Type);
        Assert.True(pi.Row.Row.IsPure);
    }

    /// <summary>A function returning its reference carries the reference's heap in its result, so the effect stays.</summary>
    [Fact]
    public void AnEscapingHeapKeepsItsEffect()
    {
        var elaborated = Elaborate("fn(u : Unit) { ref(1) }");
        var pi = Assert.IsType<Value.VPi>(elaborated.Type);
        var row = Nbe.EvalRowClosure(elaborated.Context.Metas, pi.Row, new Value.VVar(elaborated.Context.Width, []));
        Assert.NotNull(MutationEffect.HeapOf(Assert.Single(row.Effects)));
    }

    [Theory]
    [InlineData("deref(1)")]
    [InlineData("1 <- 2")]
    public void OnlyAReferenceIsReadOrWritten(string source) =>
        Assert.Equal("a reference operation on a value that is not a reference", ErrorOf(source));

    /// <summary>An unhandled Mutate names the reference, never the hidden heap.</summary>
    [Fact]
    public void AnEscapingHeapInAPureResultNamesItsEffect() =>
        Assert.Equal("effects in a pure result: Mutate; write ->{E} T or ~> T",
            ErrorOf("{ f : Unit -> Ref(I64) = fn(u : Unit) { ref(1) }; 1 }"));

    /// <summary>E6 through references: storing a closure that performs a handled effect into an outer reference escapes the handler.</summary>
    [Fact]
    public void AStoreIntoAnOuterReferenceEscapesTheHandler() =>
        Assert.Equal("a function performing Exc escapes the handler that handles it",
            ErrorOf("{ effect Exc = sig { raise : I64 -> I64 }; q = ref(fn(u : Unit) ->{Exc} I64 { 0 }); "
                    + "match (0) { x => { _ = q <- fn(u : Unit) ->{Exc} I64 { perform Exc.raise(x) }; 1 }, effect Exc.raise n => 2 } }"));

    /// <summary>Reading and writing a cell run as machine frames: a store is seen by a later read.</summary>
    [Fact]
    public void ACellHoldsWhatWasLastStored()
    {
        // let r = ref(1) in let _ = r <- 2 in deref(r)
        var term = new Term.Let(Term.U.Instance, new Term.RefNew(new Term.Atom(new Atom.I64(1))),
            new Term.Let(Term.U.Instance, new Term.RefSet(new Term.Var(0), new Term.Atom(new Atom.I64(2))),
                new Term.RefGet(new Term.Var(1))));
        Assert.Equal(new Value.VAtom(new Atom.I64(2)), Nbe.Eval(new MetaContext(), Environment.Empty, term));
    }
}
