using Fun.Compiler;

namespace Fun.Tests;

/// <summary>
/// Evidence resolution: the exact failure, so a conformance case expecting
/// `error` cannot pass through the wrong one.
/// </summary>
public class TraitTests
{
    private const string Size = "trait Size(A) = sig { size : A -> I64 }; ";
    private const string Bounded = "f : [A : Size] -> A -> I64 = fn[A : Type](x) { Size.size(x) }; f(3) }";

    private static string Elaborate(string source) =>
        Assert.Throws<FunException>(() => Driver.Elaborate(source, new Dictionary<string, string>())).Message;

    private static string Run(string source) =>
        Driver.Describe(Driver.Run(Driver.Elaborate(source, new Dictionary<string, string>())));

    [Fact]
    public void AnImplInsideAModuleIsNotEvidenceUntilOpened() =>
        Assert.Equal("missing implementation of `Size`",
            Elaborate("{ " + Size + "M = module { pub impl Size(I64) = module { size = fn(x) { 4 } } }; " + Bounded));

    [Fact]
    public void TwoDifferentImplsForOneArgumentAreAmbiguous() =>
        Assert.Equal("ambiguous implementation of `Size`",
            Elaborate("{ " + Size
                + "M = module { pub impl Size(I64) = module { size = fn(x) { 1 } } }; "
                + "N = module { pub impl Size(I64) = module { size = fn(x) { 2 } } }; open M; open N; " + Bounded));

    /// <summary>Opening one module twice brings its impl once: the same impl is not an ambiguity.</summary>
    [Fact]
    public void OpeningAModuleTwiceIsNotAnAmbiguity() =>
        Assert.Equal("4",
            Run("{ " + Size + "M = module { pub impl Size(I64) = module { size = fn(x) { 4 } } }; open M; open M; " + Bounded));

    /// <summary>A dictionary is resolved by its argument: an impl for another type is not evidence.</summary>
    [Fact]
    public void AnImplForAnotherArgumentIsNotEvidence() =>
        Assert.Equal("missing implementation of `Size`",
            Elaborate("{ " + Size + "impl Size(Char) = module { size = fn(x) { 1 } }; " + Bounded));

    [Fact]
    public void AnImplMustGiveEveryOperation() =>
        Assert.Equal("missing trait field `size`", Elaborate("{ " + Size + "impl Size(I64) = module { }; 0 }"));

    private const string TwoImpls = "impl Size(I64) = module { size = fn(n) { 1 } }; impl Size(Char) = module { size = fn(c) { 2 } }; ";

    /// <summary><c>Trait.op</c> chooses by the argument's type, not by which impl is nearest.</summary>
    [Fact]
    public void TraitOpChoosesByArgumentType() =>
        Assert.Equal("1", Run("{ " + Size + TwoImpls + "Size.size(5) }"));

    /// <summary>Nearness never breaks a tie: <c>Trait.op</c> meets the same ambiguity a bound does.</summary>
    [Fact]
    public void NearnessDoesNotBreakATie() =>
        Assert.Equal("ambiguous implementation of `Size`",
            Elaborate("{ " + Size + "impl Size(I64) = module { size = fn(n) { 1 } }; "
                + "M = module { pub impl Size(I64) = module { size = fn(n) { 2 } } }; open M; Size.size(5) }"));

    /// <summary>A choice waits for an argument type the rest of the unit solves.</summary>
    [Fact]
    public void AChoiceWaitsForItsArgumentType() =>
        Assert.Equal("2", Run("{ " + Size + TwoImpls + "(fn(x) { Size.size(x) })('c') }"));

    /// <summary>An argument type nothing ever solves is an error at the end of the unit, never a guess.</summary>
    [Fact]
    public void AnArgumentTypeNeverKnownIsAnError() =>
        Assert.Equal("cannot choose an implementation of `Size`: its argument type is never known",
            Elaborate("{ " + Size + TwoImpls + "(fn(x) { Size.size(x) }) }"));
}
