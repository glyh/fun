using Fun.Kernel;

namespace Fun.Tests;

public class EquatableArrayTests
{
    /// <summary>
    /// Records compare their fields with Equals; an ImmutableArray field compares
    /// by reference, which made two identical tuples unequal.
    /// </summary>
    [Fact]
    public void RecordsHoldingSequencesCompareStructurally()
    {
        Term Tuple() => new Term.Prod([new Term.Atom(new Atom.I64(1)), new Term.Var(0)]);

        Assert.Equal(Tuple(), Tuple());
        Assert.Equal(Tuple().GetHashCode(), Tuple().GetHashCode());
        Assert.NotEqual(Tuple(), new Term.Prod([new Term.Var(0)]));
    }

    [Fact]
    public void DefaultIsEmpty()
    {
        EquatableArray<int> unset = default;

        Assert.True(unset.IsEmpty);
        Assert.Equal(EquatableArray<int>.Empty, unset);
    }
}
