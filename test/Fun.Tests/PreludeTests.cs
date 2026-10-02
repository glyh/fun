using Fun.Compiler;
using Fun.Kernel;

namespace Fun.Tests;

/// <summary>
/// The prelude is the <c>std</c> unit, which imports the bootstrap and the library
/// units and re-exports them. They all reach a program through its
/// <c>open (import "std")</c>, and a failure is the program's own error - nothing is
/// held back as "not ported yet".
/// </summary>
public class PreludeTests
{
    private static Exception Failure(string source) =>
        Assert.ThrowsAny<Exception>(() => Driver.Run(Driver.Elaborate(source, new Dictionary<string, string>())));

    [Fact]
    public void StdIsTheUnitWhichReExportsTheBootstrap() =>
        Assert.Equal("Some", Driver.Describe(Driver.Run(Driver.Elaborate(
            "if (not(False)) { Option.Some(1 + 1) } else { Option.None }", new Dictionary<string, string>()))));

    /// <summary>The bootstrap is its own unit: <c>"std"</c> names the prelude and nothing else.</summary>
    [Fact]
    public void TheBootstrapIsNotImportableAsStd() =>
        Assert.Equal("import not found: \"std/bootstrap\"",
            Assert.IsType<FunException>(Failure("{ S = import \"std/bootstrap\"; 1 }")).Message);

    /// <summary>
    /// Every name the compiler spells for the prelude is declared once
    /// (<see cref="PreludeAbi"/>) and resolves against the loaded prelude. The same
    /// check runs as the bootstrap loads; this is the half <c>dotnet test</c> reaches.
    /// </summary>
    [Fact]
    public void TheDeclaredBootstrapInterfaceResolves() => Prelude.VerifyAbi();

    [Theory]
    [InlineData("{ x = 1; y }", "unbound variable: y while inferring the form at <unknown>:1:9-1:10")]
    [InlineData("f (1)", "function call must be adjacent to the callee; whitespace application is not supported")]
    public void AFailureIsTheProgramsOwnError(string source, string message) =>
        Assert.Equal(message, Assert.IsAssignableFrom<FunException>(Failure(source)).Message);
}
