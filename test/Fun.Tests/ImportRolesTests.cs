using Fun.Compiler;
using Fun.Expand;
using Fun.Kernel;

namespace Fun.Tests;

public class ImportRolesTests
{
    private static readonly Dictionary<string, string> Ops = new() { ["ops"] = "pub syntax answer { answer => 42 }" };

    private static string ErrorOf(string source, Dictionary<string, string>? units = null) =>
        Assert.Throws<FunException>(() => Driver.Elaborate(source, units ?? [])).Message;

    [Theory]
    [InlineData("{ M = module { pub answer = 7 }; syntax answer { answer => 42 }; open M; 1 }")]
    [InlineData("{ M = module { pub answer = 7 }; open M; syntax answer { answer => 42 }; 1 }")]
    public void AnOpenMayNotSupplyARoleName(string source) =>
        Assert.Equal("the open supplies `answer`, which a syntactic role names in its region", ErrorOf(source));

    [Fact]
    public void AUnitsOpenMayNotSupplyItsOwnRoleName() =>
        Assert.Equal("the open supplies `answer`, which a syntactic role names in its region",
            ErrorOf("{ U = import \"user\"; U.r }", new()
            {
                ["m_answer"] = "pub answer = 7",
                ["user"] = "syntax answer { answer => 42 };\nopen (import \"m_answer\");\npub r = 1",
            }));

    /// <summary>The open that brought a role delivers its own name without conflict.</summary>
    [Fact]
    public void AUnitsOwnRoleIsNotNotedAgainstItsOpen()
    {
        var expanded = Expander.ExpandExpr("{ open (import \"ops\"); 1 }", new Loader(Ops));
        var open = Assert.IsType<Syntax.Open>(expanded);
        Assert.Equal("unit:ops", open.Label);
        Assert.DoesNotContain("answer", open.RolesInRegion);
    }

    /// <summary>After the region of the open that imported it, the role's name is a plain name again.</summary>
    [Fact]
    public void AnImportedRoleStaysInItsRegion()
    {
        var expanded = Expander.ExpandExpr("{ x = { open (import \"ops\"); answer }; answer }", new Loader(Ops));
        var let = Assert.IsType<Syntax.Let>(expanded);
        var inside = Assert.IsType<Syntax.Open>(let.Value);
        Assert.Equal(new Syntax.Atom(new Atom.I64(42), inside.Body.Span), inside.Body);
        var after = Assert.IsType<Syntax.OpenChoice>(let.Body);
        Assert.Equal("answer", after.Name.Name);
        Assert.Empty(after.Opens);
    }

    [Fact]
    public void AUnitIsExpandedOnce()
    {
        var loader = new Loader(Ops);
        Assert.Same(loader.LoadSyntax("ops").Roles.Single().Role, loader.LoadSyntax("ops").Roles.Single().Role);
    }
}
