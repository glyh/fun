using Fun.Compiler;
using Fun.Kernel;

namespace Fun.Tests;

/// <summary>
/// The unreachable-arm check (rule 8 of a-pattern-binder-is-lowercase): structural
/// subsumption, no value reasoning. Not yet wired into elaboration, so it is pinned
/// here rather than by a conformance case.
/// </summary>
public class UnreachableArmTests
{
    private static CorePattern I64(long n) => new CorePattern.Atom(new Atom.I64(n));
    private static CorePattern Wild => CorePattern.Wild.Instance;
    private static CorePattern Bind => CorePattern.Bind.Instance;
    private static CorePattern Con(string name, params CorePattern[] args) => new CorePattern.Con(name, 0, [.. args]);
    private static CorePattern Record(bool partial, params (string, CorePattern)[] fields) => new CorePattern.Record([.. fields], partial);

    /// <summary>A later arm with the same literal is covered by the earlier one.</summary>
    [Fact]
    public void TheSameLiteralTwiceIsAlreadyAFailure() =>
        Assert.Equal(1, MatchCompile.UnreachableArm([I64(0), I64(0)]));

    /// <summary>A wildcard wholesale covers every later arm.</summary>
    [Fact]
    public void AWildcardCoversALaterLiteral() =>
        Assert.Equal(1, MatchCompile.UnreachableArm([Wild, I64(0)]));

    /// <summary>A binder at a payload position covers the same head with a literal there.</summary>
    [Fact]
    public void ABinderAtAPositionCoversALaterLiteralThere()
    {
        Assert.Equal(1, MatchCompile.UnreachableArm([Con("Some", Bind), Con("Some", I64(0))]));
        // The other direction is not subsumption: the literal does not cover the binder.
        Assert.Null(MatchCompile.UnreachableArm([Con("Some", I64(0)), Con("Some", Bind)]));
    }

    /// <summary>A later or-pattern is covered when an earlier arm covers each alternative.</summary>
    [Fact]
    public void OrPatternsSplit()
    {
        Assert.Equal(2, MatchCompile.UnreachableArm([I64(0), I64(1), new CorePattern.Or(I64(0), I64(1))]));
        Assert.Null(MatchCompile.UnreachableArm([I64(0), new CorePattern.Or(I64(0), I64(2))]));
    }

    /// <summary>A closed record does not cover a partial one: the partial matches more.</summary>
    [Fact]
    public void AClosedRecordDoesNotCoverAPartialOne()
    {
        Assert.Null(MatchCompile.UnreachableArm(
            [Record(false, ("x", I64(0))), Record(true, ("x", I64(0)))]));
        // A partial pattern with a wildcard field covers a closed one with an equal field.
        Assert.Equal(1, MatchCompile.UnreachableArm(
            [Record(true, ("x", Wild)), Record(false, ("x", I64(0)))]));
    }

    /// <summary>A wildcard arm after a literal is fine; nothing was covered.</summary>
    [Fact]
    public void ALiteralThenAWildcardIsFine() =>
        Assert.Null(MatchCompile.UnreachableArm([I64(0), Wild]));
}
