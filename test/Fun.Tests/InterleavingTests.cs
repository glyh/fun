using Fun.Compiler;

namespace Fun.Tests;

public class InterleavingTests
{
    private static Elaborated WithUnit(string program, string unit) =>
        Driver.Elaborate(program, new Dictionary<string, string> { ["u"] = unit });

    /// <summary>
    /// A macro body compiles against its unit as of its definition (M3): a binding
    /// written after it does not exist yet.
    /// </summary>
    [Fact]
    public void AMacroBodyDoesNotSeeALaterBinding()
    {
        var error = Assert.Throws<FunException>(() => WithUnit("{ open (import \"u\"); five(0) }",
            "open (import \"std\"); pub macro five(_) { Syntax.i64(helper(5)) }; pub helper = fn(x : I64) { x };"));
        Assert.Equal("unbound variable: helper", error.Message);
    }

    [Fact]
    public void AMacroBodySeesAnEarlierBinding() =>
        Assert.Equal("5", Driver.Describe(Driver.Run(WithUnit("{ open (import \"u\"); five(0) }",
            "open (import \"std\"); pub helper = fn(x : I64) { x }; pub macro five(_) { Syntax.i64(helper(5)) };"))));
}
