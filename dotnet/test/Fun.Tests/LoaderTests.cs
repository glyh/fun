using Fun.Compiler;

namespace Fun.Tests;

public class LoaderTests
{
    private static Loader With(params (string Path, string Source)[] units) =>
        new(units.ToDictionary(u => u.Path, u => u.Source));

    /// <summary>A unit imported twice is elaborated once: both imports get the same value.</summary>
    [Fact]
    public void ElaboratesAUnitOnce()
    {
        var loader = With(("m", "pub v = 5"));
        var metas = new MetaContext();

        Assert.Same(loader.Load("m", metas).Value, loader.Load("m", metas).Value);
    }

    [Fact]
    public void ACycleIsAnError()
    {
        var loader = With(("a", "B = import \"b\"; pub x = 1"), ("b", "A = import \"a\"; pub y = 2"));

        var error = Assert.Throws<FunException>(() => loader.Load("a", new MetaContext()));
        Assert.Contains("circular import", error.Message);
    }

    /// <summary>A unit sees the base context, not its importer's names, and no prelude it did not open.</summary>
    [Fact]
    public void AUnitIsStrict()
    {
        var error = Assert.Throws<FunException>(() => With(("u", "pub v = outer_val")).Load("u", new MetaContext()));
        Assert.Contains("unbound variable: outer_val", error.Message);
    }

    /// <summary>
    /// <c>import "std"</c> is the one prelude every loader shares, elaborated once per
    /// process, and the base context binds that same value as <c>stdlib</c>.
    /// </summary>
    [Fact]
    public void ThePreludeIsElaboratedOnceAndBoundAsStdlib()
    {
        var first = With().Load("std", new MetaContext());
        Assert.Same(first.Value, With().Load("std", new MetaContext()).Value);

        var ctx = Elaborator.BaseContext(new MetaContext(), preludeOpen: false);
        var (index, _) = ctx.Locate("stdlib");
        Assert.Same(first.Value, ctx.Environment[index]);
    }

    /// <summary>A base context's metas start after the prelude's, so no id means two metas.</summary>
    [Fact]
    public void ABaseContextsMetasFollowThePreludes()
    {
        var metas = new MetaContext();
        Elaborator.BaseContext(metas, preludeOpen: false);
        Assert.True(metas.Count > 0);
        Assert.Equal(metas.Count, metas.Fresh());
    }

    [Fact]
    public void AMissingUnitIsAnError() =>
        Assert.Throws<FunException>(() => With().Load("missing", new MetaContext()));
}
