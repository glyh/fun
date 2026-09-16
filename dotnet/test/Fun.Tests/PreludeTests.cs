using Fun.Compiler;

namespace Fun.Tests;

/// <summary>
/// Stage 1 of the prelude is ported and stage 2 is not: the runner must see a
/// missing stage-2 name or role as "not ported yet", and anything else as the
/// program's own error.
/// </summary>
public class PreludeTests
{
    private static Exception Failure(string source) =>
        Assert.ThrowsAny<Exception>(() => Driver.Run(Driver.Elaborate(source, new Dictionary<string, string>())));

    [Fact]
    public void Stage2NamesAreReadOffItsSource()
    {
        Assert.Superset(new HashSet<string> { "not", "==", "!=", "&&", "+", "Eq", "type", "additive", "type_decls" },
            new HashSet<string>(Prelude.Stage2Names));
        // Stage 1's names are ported: they are not stage 2's to supply.
        Assert.DoesNotContain("i64_to_bool", Prelude.Stage2Names);
    }

    [Fact]
    public void AProgramSeesStage1ThroughTheOpenOfStd() =>
        Assert.Equal("Some", Driver.Describe(Driver.Run(Driver.Elaborate(
            "if (True) { Option.Some(1) } else { Option.None }", new Dictionary<string, string>()))));

    [Theory]
    [InlineData("not(True)")]                     // a stage-2 value
    [InlineData("{ x = 1; y == x }")]             // a stage-2 operator: its role is missing too
    [InlineData("{ type Color = Red; 1 }")]       // a stage-2 syntax form
    public void AMissingStage2NameIsNotPortedYet(string source) =>
        Assert.IsType<NotImplementedException>(Failure(source));

    [Theory]
    [InlineData("{ x = 1; y }", "unbound variable: y")]
    [InlineData("f (1)", "function call must be adjacent to the callee; whitespace application is not supported")]
    public void AnythingElseIsTheProgramsOwnError(string source, string message) =>
        Assert.Equal(message, Assert.IsType<FunException>(Failure(source)).Message);
}
