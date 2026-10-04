using Quill.Compiler;

namespace Quill.Tests;

/// <summary>
/// Every elaborator error names the form it was at, exactly once: the position is the
/// innermost enclosing form however deeply the error is nested (the <c>Located</c> marker
/// <c>Elaborator.At</c> throws), never once per enclosing frame.
/// </summary>
public class ErrorPositionTests
{
    [Fact]
    public void AnErrorNestedSeveralFormsDeepNamesTheInnermostFormOnce()
    {
        // `y` is the innermost form, under the let, the application f(...) and the
        // application g(...) -- three enclosing At frames a naive suffix-append would repeat.
        var error = Assert.ThrowsAny<FunException>(() => Driver.Elaborate(
            "{ f = fn(a : I64) { a }; g = fn(b : I64) { b }; f(g(y)) }", new Dictionary<string, string>()));
        const string suffix = " while inferring the form at ";
        Assert.Equal(1, error.Message.Split(suffix).Length - 1);
        Assert.EndsWith("at <unknown>:1:52-1:53", error.Message);
    }
}
