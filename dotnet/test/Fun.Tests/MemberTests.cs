using Fun.Compiler;
using Fun.Kernel;

namespace Fun.Tests;

/// <summary>
/// A container's public member names are unique (decision 3 of the divergence review).
/// The rule reads the container's <em>public</em> entries, so a private binding may still
/// shadow a public one, and an <c>open</c> supplies no members of this container.
/// </summary>
public class MemberTests
{
    private static readonly Dictionary<string, string> NoUnits = [];

    private static string ErrorOf(string source) =>
        Assert.Throws<FunException>(() => Driver.Elaborate(source, NoUnits)).Message;

    [Fact]
    public void ARepeatedPublicMemberIsRejected() =>
        Assert.Equal("duplicate member: `x` is already public",
            ErrorOf("{ M = module { pub x = 1; pub x = 2 }; M.x }"));

    [Fact]
    public void ARepeatedConstructorIsRejected() =>
        Assert.Equal("duplicate constructor `A`",
            ErrorOf("{ M = module { pub rec E = enum { A(I64), A } }; 0 }"));

    /// <summary>A private rebinding is not a member, so it shadows rather than clashes.</summary>
    [Fact]
    public void APrivateBindingMayShadowAPublicOne() =>
        Assert.DoesNotContain("duplicate",
            Driver.Describe(Driver.Run(Driver.Elaborate(
                "{ M = module { pub x = 1; x = 2; pub y = x }; M.y * 10 + M.x }", NoUnits))));

    /// <summary>An open exports nothing, so its names are not members here.</summary>
    [Fact]
    public void AnOpenNameMayBeShadowedByAPublicMember() =>
        Assert.Equal("2", Driver.Describe(Driver.Run(Driver.Elaborate(
            "{ N = module { pub x = 1 }; M = module { open N; pub x = 2 }; M.x }", NoUnits))));
}
