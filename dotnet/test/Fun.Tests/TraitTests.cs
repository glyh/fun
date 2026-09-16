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
}
