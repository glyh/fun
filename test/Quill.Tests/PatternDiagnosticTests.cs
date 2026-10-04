using Quill.Compiler;

namespace Quill.Tests;

/// <summary>
/// The reworded pattern diagnostics (a-pattern-binder-is-lowercase, stage 1). xUnit
/// rather than a conformance case for the reason test/conformance/cases/README.md
/// gives: a shared case can only say <c>error</c>, and the point of these is *which*
/// error.
/// </summary>
public class PatternDiagnosticTests
{
    private static string Failure(string source) =>
        Assert.ThrowsAny<FunException>(() => Driver.Run(Driver.Elaborate(source, new Dictionary<string, string>()))).Message;

    /// <summary>A bare type-case head that names no type says type/nominal, not constructor.</summary>
    [Fact]
    public void ABareHeadThatNamesNoTypeSaysTypeOrNominal() =>
        Assert.Contains("`C` is not a type or nominal in scope", Failure(
            "{ C = 5; classify = fn(T : Type) { match (T) { C => 1, _ => 0 } }; classify(I64) }"));

    /// <summary>An applied type-case head that names no type names the argument at fault.</summary>
    [Fact]
    public void AnAppliedHeadThatNamesNoTypeNamesTheTerm() =>
        Assert.Contains("`Some` must name a type in a type-case", Failure(
            "{ classify = fn(T : Type) { match (T) { Option(Some(1)) => 1, _ => 0 } }; classify(Option(I64)) }"));

    /// <summary>A pattern followed by another term names the token it stopped at.</summary>
    [Fact]
    public void UnexpectedTermsAfterAPatternNameTheToken() =>
        Assert.Contains("unexpected terms after the pattern: Bool", Failure(
            "{ classify = fn(T : Type) { match (T) { I64 Bool => 1, _ => 0 } }; classify(I64) }"));

    /// <summary>An arm an earlier arm subsumes is an unreachable-arm error.</summary>
    [Fact]
    public void AnArmAnEarlierArmSubsumesIsAnError() =>
        Assert.Contains("unreachable match arm 2: an earlier arm covers it", Failure(
            "{ classify = fn(v : Option(I64)) { match (v) { Some(a) => 1, Some(1) => 2, _ => 0 } }; classify(Some(1)) }"));
}
