using Fun.Compiler;
using Fun.Kernel;

namespace Fun.Tests;

/// <summary>
/// Procedural macros' internals: the errors a call is checked for before the macro
/// runs (M8), quote holes' kinds, and budget accounting (M5).
/// </summary>
public class MacroTests
{
    private static Exception Failure(string source) =>
        Assert.ThrowsAny<Exception>(() => Driver.Run(Driver.Elaborate(source, new Dictionary<string, string>())));

    private static string Run(string source) =>
        Driver.Describe(Driver.Run(Driver.Elaborate(source, new Dictionary<string, string>())));

    [Theory]
    [InlineData("{ macro d(_) : Decl { quote { x = 1 } }; d(0) }",
        "macro d returns declarations but is used where an expression goes")]
    [InlineData("{ macro m(a) { a }; m(1, 2) }", "macro m takes 1 arguments, the call gives 2")]
    [InlineData("{ macro m(n : Id) { Syntax.RawVar(None, n) }; m(1) }", "macro m takes Id here")]
    [InlineData("{ macro m(_) { 5 }; m(0) }", "macro m did not return syntax")]
    [InlineData("{ macro m(x) { quote(fn($x) { $x }) }; 1 }", "the quote hole $x stands in positions of different kinds")]
    [InlineData("{ macro d[A](_) : Expr(A) { { _ = A; Syntax.i64(1) } }; d(0) }", "the type binder A of macro d is not solved at its call")]
    [InlineData("{ macro m(x : Expr(I64)) : Expr(I64) { x }; m(True) }", "type mismatch in the argument x of macro m: ")]
    [InlineData("{ macro m(_) : Expr(I64) { quote(True) }; m(0) }", "type mismatch in the output of macro m: ")]
    [InlineData("{ macro m(_) : Expr(I64) { Syntax.i64(1) }; x : Bool = m(0); x }", "type mismatch: ")]
    public void ACallIsCheckedBeforeTheMacroRuns(string source, string message) =>
        Assert.StartsWith(message, Assert.IsType<FunException>(Failure(source)).Message);

    /// <summary>A macro application is a call under the one budget: a divergent body is an error naming it, not a hang.</summary>
    [Fact]
    public void ADivergentMacroRunsOutOfBudget()
    {
        var message = Assert.IsType<FunException>(Failure(
            "{ macro spin(_) { rec f = fn(n : I64) : I64 { f(n) }; _ = f(0); Syntax.i64(1) }; spin(0) }")).Message;
        Assert.Contains("exceeded the budget", message);
        Assert.Contains("macro 'spin'", message);
    }

    /// <summary>expand_decls expands a declaration list as a context of its own: what it binds does not leak.</summary>
    [Fact]
    public void ExpandDeclsBindsNothingOutsideTheResult() =>
        Assert.Equal("unbound variable: hidden", Assert.IsType<FunException>(Failure(
            "{ macro count(d : List(Decl)) { _ = Syntax.expand_decls(d); Syntax.i64(1) }; _ = count({ hidden = 1 }); hidden }")).Message);

    [Fact]
    public void AQuoteFillsItsHoles() =>
        Assert.Equal("7", Run("{ macro k(e) { quote((fn(y) { y })($e)) }; k(7) }"));

    [Fact]
    public void MacroFormsRoundTripThroughReflection()
    {
        var r = Reflection.OfPrelude;
        var id = new Id("m", SourceSpan.Synthetic);
        var hole = new Syntax.Var(new Id("$e", SourceSpan.Synthetic));
        Syntax quote = new Syntax.Quote(hole, [("$e", new Syntax.Var(new Id("e", SourceSpan.Synthetic)))], SourceSpan.Synthetic);
        Syntax def = new Syntax.MacroDef(id, quote, new Syntax.MacroCall(new Syntax.Var(id),
            [new Capture.Expr(new Syntax.Stx(hole, SourceSpan.Synthetic)), new Capture.Tokens([])], SourceSpan.Synthetic),
            FormKind.Expr, new Syntax.Var(new Id("I64", SourceSpan.Synthetic)), SourceSpan.Synthetic);
        Assert.Equal(def, r.ReadExpr(r.ReflectExpr(def)));

        Binding macro = new Binding.Macro(id, new Syntax.QuoteDecls([new Binding.Hole(new Id("$d", SourceSpan.Synthetic))], [], SourceSpan.Synthetic), true, FormKind.Decl, null);
        Assert.Equal(macro, r.ReadDecl(r.ReflectDecl(macro)));
    }
}
