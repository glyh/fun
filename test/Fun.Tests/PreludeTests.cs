using Fun.Compiler;

namespace Fun.Tests;

/// <summary>
/// The prelude is stage 2, which imports stage 1 as its own unit and re-exports it.
/// Both stages reach a program through its <c>open (import "std")</c>, and a failure
/// is the program's own error - nothing is held back as "not ported yet".
/// </summary>
public class PreludeTests
{
    private static Exception Failure(string source) =>
        Assert.ThrowsAny<Exception>(() => Driver.Run(Driver.Elaborate(source, new Dictionary<string, string>())));

    [Fact]
    public void StdIsStage2WhichReExportsStage1() =>
        Assert.Equal("Some", Driver.Describe(Driver.Run(Driver.Elaborate(
            "if (not(False)) { Option.Some(1 + 1) } else { Option.None }", new Dictionary<string, string>()))));

    /// <summary>Stage 1 is its own unit: <c>"std"</c> names stage 2 and nothing else.</summary>
    [Fact]
    public void Stage1IsNotImportableAsStd() =>
        Assert.Equal("import not found: \"std/stage1\"",
            Assert.IsType<FunException>(Failure("{ S = import \"std/stage1\"; 1 }")).Message);

    [Theory]
    [InlineData("{ x = 1; y }", "unbound variable: y")]
    [InlineData("f (1)", "function call must be adjacent to the callee; whitespace application is not supported")]
    public void AFailureIsTheProgramsOwnError(string source, string message) =>
        Assert.Equal(message, Assert.IsType<FunException>(Failure(source)).Message);
}
