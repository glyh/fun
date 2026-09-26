using Fun.Compiler;
using Fun.Expand;
using Fun.Kernel;

namespace Fun.Tests;

public class RolesTests
{
    private static Order Group(string name, Assoc assoc = Assoc.Left, bool weakest = false,
        EquatableArray<Order> stronger = default, EquatableArray<Order> weaker = default) =>
        new($"{name}@test", name, assoc, weakest, stronger, weaker);

    [Fact]
    public void OrderIsTransitiveAndWeakestYieldsToStatedRelations()
    {
        var low = Group("low");
        var mid = Group("mid", stronger: [low]);
        var high = Group("high", stronger: [mid]);
        var bottom = Group("bottom", weakest: true);

        Assert.Equal(OrderRelation.Stronger, Order.Relation(high, low));
        Assert.Equal(OrderRelation.Weaker, Order.Relation(low, high));
        Assert.Equal(OrderRelation.Unrelated, Order.Relation(low, Group("other")));
        Assert.Equal(OrderRelation.Weaker, Order.Relation(bottom, low));
        // A stated relation wins over weakest.
        Assert.Equal(OrderRelation.Stronger, Order.Relation(bottom, Group("below", weaker: [bottom])));
    }

    [Fact]
    public void RolesResolveByScopeSetAmongTheirFixity()
    {
        var table = new BinderTable();
        var outer = new Role(Fixity.Prefix, null, RoleMeaning.ApplyValue.Instance, SourceSpan.Synthetic, null);
        var inner = outer with { Fixity = Fixity.Prefix, Meaning = RoleMeaning.CallMacro.Instance };
        table.Extend("f", ScopeSet.Of([1]), "f", BinderMeaning.Role, outer);
        table.Extend("f", ScopeSet.Of([1, 2]), "f", BinderMeaning.Role, inner);

        Assert.Same(inner, table.FindRole("f", Fixity.Prefix, ScopeSet.Of([1, 2, 3])));
        Assert.Same(outer, table.FindRole("f", Fixity.Prefix, ScopeSet.Of([1])));
        Assert.Null(table.FindRole("f", Fixity.Infix, ScopeSet.Of([1, 2])));
        // A role is never a value occurrence's binder.
        Assert.Null(table.Resolve(new Id("f", SourceSpan.Synthetic, ScopeSet.Of([1, 2]))));
    }

    /// <summary>The error cases in the shared suite fail with the role error itself, not a missing form.</summary>
    [Theory]
    [InlineData("{ syntax answer { answer => 42 }; answer = 5; answer }", "both a syntactic role and a value binder")]
    [InlineData("{ order a; order b; infix (@@) a ($x, $y) { $x }; infix (%%) b ($x, $y) { $y }; 1 @@ 2 %% 3 }", "have no declared order")]
    [InlineData("{ order once : assoc(none); infix (@@) once ($a, $b) { $a }; 1 @@ 2 @@ 3 }", "do not chain")]
    public void GenuineRoleErrors(string source, string message) =>
        Assert.Contains(message, Assert.Throws<FunException>(() => Driver.Elaborate(source, new Dictionary<string, string>())).Message);
}
