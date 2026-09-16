using Fun.Compiler;
using Fun.Kernel;

namespace Fun.Tests;

public class MatchCompileTests
{
    private static readonly Occurrence Root = Occurrence.Base.Instance;
    private static Occurrence At(int i) => new Occurrence.Child(Root, i);
    private static CorePattern I64(long n) => new CorePattern.Atom(new Atom.I64(n));
    private static CorePattern Bind => CorePattern.Bind.Instance;
    private static CorePattern Wild => CorePattern.Wild.Instance;

    private static MatchDomain I64Domain(Occurrence _) => new MatchDomain.AtomDomain(AtomTy.I64);

    /// <summary>
    /// <c>(a, 1) => …, (b, _) => …</c>: the refutable second column is tested
    /// first, yet each leaf binds in source order.
    /// </summary>
    [Fact]
    public void TestsTheRefutableColumnAndKeepsSourceBindingOrder()
    {
        var (tree, missing) = MatchCompile.Compile(
            [new CorePattern.Prod([Bind, I64(1)]), new CorePattern.Prod([Bind, Wild])], I64Domain);

        Assert.Null(missing);
        Assert.Equal(
            new DecisionTree.Switch(At(1),
                [new SwitchCase(new Atom.I64(1), new DecisionTree.Leaf(0, [At(0)]))],
                new DecisionTree.Leaf(1, [At(0)])),
            tree);
    }

    /// <summary>An or-pattern is one row per alternative, all leading to its arm.</summary>
    [Fact]
    public void ExpandsOrPatterns()
    {
        var (tree, _) = MatchCompile.Compile([new CorePattern.Or(I64(0), I64(1)), Wild], I64Domain);

        Assert.Equal(
            new DecisionTree.Switch(Root,
                [new SwitchCase(new Atom.I64(0), new DecisionTree.Leaf(0, [])), new SwitchCase(new Atom.I64(1), new DecisionTree.Leaf(0, []))],
                new DecisionTree.Leaf(1, [])),
            tree);
    }

    [Fact]
    public void AnI64MatchWithoutAFallbackMissesEverythingElse()
    {
        var (tree, missing) = MatchCompile.Compile([I64(1)], I64Domain);

        Assert.Null(tree);
        Assert.Equal("_", missing!.ToString());
    }

    /// <summary>Unit has one atom, so matching it is exhaustive.</summary>
    [Fact]
    public void UnitIsCoveredByItsOneAtom()
    {
        var (tree, missing) = MatchCompile.Compile(
            [new CorePattern.Atom(Atom.Unit.Instance)], _ => new MatchDomain.AtomDomain(AtomTy.Unit));

        Assert.Null(missing);
        Assert.NotNull(tree);
    }

    [Fact]
    public void NamesTheConstructorNoArmMatches()
    {
        MatchDomain Option(Occurrence _) => new MatchDomain.Nominal(
            [new ConstructorShape("Some", 1, 1), new ConstructorShape("None", 1, 0)]);

        var (_, missing) = MatchCompile.Compile([new CorePattern.Con("Some", 1, [Wild])], Option);

        Assert.Equal("None", missing!.ToString());
    }
}
