using Fun.Expand;
using Fun.Kernel;

namespace Fun.Tests;

public class ExpandTests
{
    /// <summary>Expanded syntax as an s-expression, with binders' resolved names.</summary>
    private static string Show(Syntax s) => s switch
    {
        Syntax.Atom a => a.Value.ToString(),
        Syntax.Var v => v.Id.Name,
        Syntax.Ap a => $"({Show(a.Fn)} {Show(a.Arg)})",
        Syntax.Lam l => $"(fn {l.Param.Name.Name}{ShowType(l.Param.Type)} {Show(l.Body)})",
        Syntax.Let l => $"(let {l.Name.Name}{ShowType(l.Type)} {Show(l.Value)} {Show(l.Body)})",
        Syntax.Annotated a => $"({Show(a.Inner)} : {Show(a.Type)})",
        Syntax.Arrow a => $"({(a.Name is null ? "" : a.Name.Name + " : ")}{Show(a.Domain)} -> {Show(a.Codomain)})",
        Syntax.Prod p => $"(tuple {string.Join(" ", p.Items.Select(Show))})",
        Syntax.Proj p => $"({Show(p.Of)}.{p.Index})",
        Syntax.FieldAccess f => $"({Show(f.Of)}.{f.Field})",
        Syntax.Block => "<block>",
        _ => throw new InvalidOperationException(s.GetType().Name),
    };

    private static string ShowType(Syntax? t) => t is null ? "" : $" : {Show(t)}";

    [Theory]
    [InlineData("42", "42")]
    // A binder is renamed; its occurrences follow it.
    [InlineData("(fn(x) { x })(7)", "((fn x#0 x#0) 7)")]
    [InlineData("{ x : I64 = 5; x }", "(let x#0 : I64 5 x#0)")]
    // The inner binder shadows the outer one, by name as well as by scope.
    [InlineData("fn(x) { fn(x) { x } }", "(fn x#0 (fn x#1 x#1))")]
    // A parameter's type is outside its own binder, so `x` there is the outer one.
    [InlineData("{ x : I64 = 5; fn(y : x) { y } }", "(let x#0 : I64 5 (fn y#1 : x#0 y#1))")]
    // A non-recursive value cannot see its own binder; a `rec` one can.
    [InlineData("{ f = fn(x) { f }; f }", "(let f#0 (fn x#1 f) f#0)")]
    [InlineData("{ rec f = fn(x) { f }; f }", "(let f#0 (fn x#1 f#0) f#0)")]
    // A result type annotates the whole function with its arrow type.
    // The arrow's domain binder is its own, not the lambda's parameter.
    [InlineData("fn(n : I64) : I64 { n }", "((fn n#0 : I64 n#0) : (n#1 : I64 -> I64))")]
    // Calls curry; a trailing statement with no binding discards its value.
    [InlineData("f(1, 2)", "((f 1) 2)")]
    [InlineData("{ 1; 2 }", "(let _#0 1 2)")]
    [InlineData("(1, 2).0", "((tuple 1 2).0)")]
    public void Expands(string source, string expected) =>
        Assert.Equal(expected, Show(Expander.ExpandExpr(source)));

    [Theory]
    // Whitespace application is not the language.
    [InlineData("f (1)", "function call must be adjacent to the callee; whitespace application is not supported")]
    [InlineData("{ }", "empty block")]
    [InlineData("{ x = ; 1 }", "missing value for binding: x")]
    [InlineData("fn { 1 }", "fn requires at least one parameter list")]
    // Unported forms name themselves rather than parsing into something else.
    [InlineData("1 + 2", "not ported yet: the infix operator `+`")]
    [InlineData("match (x) { }", "not ported yet: the `match` form")]
    public void Rejects(string source, string message) =>
        Assert.Equal(message, Assert.Throws<ExpandException>(() => Expander.ExpandExpr(source)).Message);
}
