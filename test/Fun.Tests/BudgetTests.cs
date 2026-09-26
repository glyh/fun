using Fun.Compiler;

namespace Fun.Tests;

/// <summary>
/// The observable budget behaviours: a macro body's overrun names the macro, and an
/// overrun in a syntax operator's body names the operator use. These are xUnit rather
/// than conformance cases for the reason the internals-parity ruling gives -- a shared
/// case can only say <c>error</c>, and the point of these is *which* error
/// (test/conformance/cases/README.md).
/// </summary>
public class BudgetTests
{
    private static string Failure(string source) =>
        Assert.Throws<FunException>(() => Driver.Run(Driver.Elaborate(source, new Dictionary<string, string>()))).Message;

    /// <summary>
    /// A divergent body under a use of a syntax operator: the overrun is the operator
    /// application's error, naming the operator it was applying -- not only the budget.
    /// The prototype's <c>test_operator_body_error_reports_use_span</c>
    /// (<c>test/backend/test_core.ml:1407</c>) also pins the use and declaration spans;
    /// the port's budget errors name the use by the operator's name, which is the
    /// identifying part asserted here.
    /// </summary>
    [Fact]
    public void AnOperatorBodyOverrunNamesTheOperatorUse()
    {
        const string diverging = "{ rec loop : I64 -> I64 = fn(n) { loop(n) }; loop(0) }";
        var message = Failure("{\n  infix (~) (stx) { " + diverging + " };\n  1 ~ 2\n}");

        Assert.Contains("exceeded the budget", message);
        Assert.Contains("'~'", message);
    }

    /// <summary>
    /// An overrun under a nest of macro applications names the chain that spent the
    /// budget, outermost first (like a stacktrace), each frame the macro's name at the
    /// site of its application: here <c>keep</c>, which asked for <c>spin</c>'s
    /// expansion, before <c>spin</c>, whose body overran. xUnit for the same reason the
    /// operator case is: a shared case can only say <c>error</c>, and the point is
    /// *which* error (test/conformance/cases/README.md).
    /// </summary>
    [Fact]
    public void ANestOfApplicationsNamesTheCallStack()
    {
        const string source = """
            { macro spin(_) { rec loop : I64 -> I64 = fn(n) { loop(n) }; loop(0) };
              macro keep(d : List(Decl)) : List(Decl) { Syntax.expand_decls(d) };
              M = module { keep({ pub x = spin(0) }) };
              0 }
            """;
        var message = Failure(source);

        Assert.Contains("exceeded the budget", message);
        Assert.Matches("the application of macro 'keep' at [^\r\n]+", message);
        Assert.Matches("the application of macro 'spin' at [^\r\n]+", message);
        Assert.True(message.IndexOf("macro 'keep'") < message.IndexOf("macro 'spin'"),
            $"outermost frame must come first:\n{message}");
    }
}
