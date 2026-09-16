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
        Syntax.Ap a => $"({Show(a.Fn)} {Implicit(a.Explicitness, Show(a.Arg))})",
        Syntax.Lam l => $"(fn {Implicit(l.Param.Explicitness, l.Param.Name.Name + ShowType(l.Param.Type))} {Show(l.Body)})",
        Syntax.Let l => $"(let {l.Name.Name}{ShowType(l.Type)} {Show(l.Value)} {Show(l.Body)})",
        Syntax.Annotated a => $"({Show(a.Inner)} : {Show(a.Type)})",
        Syntax.TraitBoundSet b => $"{{{string.Join(", ", b.Traits.Select(Show))}}}",
        Syntax.Arrow a => $"({Implicit(a.Explicitness, (a.Name is null ? "" : a.Name.Name + " : ") + Show(a.Domain))} -> {Show(a.Codomain)})",
        Syntax.Prod p => $"(tuple {string.Join(" ", p.Items.Select(Show))})",
        Syntax.Proj p => $"({Show(p.Of)}.{p.Index})",
        Syntax.FieldAccess f => $"({Show(f.Of)}.{f.Field})",
        Syntax.Block => "<block>",
        // A name no binder took and no open may supply is located in the base context.
        Syntax.OpenChoice { Opens.IsEmpty: true, Fallback: null } c => c.Name.Name,
        Syntax.OpenChoice c => $"(choice {c.Name.Name} [{string.Join(" ", c.Opens)}] {c.Fallback ?? "-"})",
        Syntax.Module m => $"(module{string.Concat(m.Bindings.Select(b => " " + ShowBinding(b)))})",
        Syntax.Open o => $"(open {Show(o.Of)} {o.Label} {Show(o.Body)})",
        _ => throw new InvalidOperationException(s.GetType().Name),
    };

    private static string ShowType(Syntax? t) => t is null ? "" : $" : {Show(t)}";

    /// <summary>An implicit binder, argument or domain is bracketed, as written.</summary>
    private static string Implicit(Explicitness explicitness, string shown) =>
        explicitness == Explicitness.Implicit ? $"[{shown}]" : shown;

    private static string ShowBinding(Binding b) => b switch
    {
        Binding.Let l => $"({(l.Public ? "pub " : "")}{l.Name.Name} {Show(l.Value)})",
        Binding.Open o => $"(open {Show(o.Of)} {o.Label})",
        Binding.Export e => $"(export {Show(e.Of)}{(e.Names is { } n ? " {" + string.Join(" ", n) + "}" : "")})",
        _ => throw new InvalidOperationException(b.GetType().Name),
    };

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
    // Module items see the items before them; an open's region makes a bare name an open choice.
    [InlineData("module { a = 1; pub b = a }", "(module (a#0 1) (pub b#1 a#0))")]
    // An export binds nothing; a trailing `.{ … }` is its selection, not a record.
    [InlineData("module { a = 1; export a.{x, y}; export a }", "(module (a#0 1) (export a#0 {x y}) (export a#0))")]
    // `y` is inside the open and its binder is not: the open is tried first, the binder is the fallback.
    [InlineData("{ M = module { pub y = 5 }; y = 1; open M; y }", "(let M#0 (module (pub y#1 5)) (let y#2 1 (open M#0 open:3 (choice y [open:3] y#2))))")]
    // `y`'s binder is inside the open, so it shadows the open.
    [InlineData("{ M = module { pub y = 5 }; open M; y = 1; y }", "(let M#0 (module (pub y#1 5)) (open M#0 open:2 (let y#2 1 y#2)))")]
    // `[A : {Eq, Show}]`: an implicit binder's bound set; each trait is an occurrence.
    [InlineData("fn[A : {Eq, Show}](a : A) { a }", "(fn [A#0 : {Eq, Show}] (fn a#1 : A#0 a#1))")]
    public void Expands(string source, string expected) =>
        Assert.Equal(expected, Show(Expander.ExpandExpr(source)));

    [Theory]
    // Implicit parameters precede the explicit ones; each is a binder the rest sees.
    [InlineData("fn[A : Type](a : A) { a }", "(fn [A#0 : Type] (fn a#1 : A#0 a#1))")]
    [InlineData("fn[A : Type] { 1 }", "(fn [A#0 : Type] 1)")]
    // `[A : Type] -> …` is one implicit arrow per binder, its name scoping over the rest.
    [InlineData("[A : Type, B : Type] -> A -> B", "([A#0 : Type] -> ([B#1 : Type] -> (A#0 -> B#1)))")]
    // A result type needs every parameter's type, implicit ones included.
    [InlineData("fn[A : Type](a : A) : A { a }", "((fn [A#0 : Type] (fn a#1 : A#0 a#1)) : ([A#2 : Type] -> (a#3 : A#2 -> A#2)))")]
    // `f[I64]` supplies an implicit argument; it may be followed by an explicit call.
    [InlineData("f[I64, Unit](1)", "(((f [I64]) [Unit]) 1)")]
    public void ExpandsImplicits(string source, string expected) =>
        Assert.Equal(expected, Show(Expander.ExpandExpr(source)));

    [Theory]
    // Whitespace application is not the language.
    [InlineData("f (1)", "function call must be adjacent to the callee; whitespace application is not supported")]
    [InlineData("{ }", "empty block")]
    [InlineData("{ x = ; 1 }", "missing value for binding: x")]
    [InlineData("fn { 1 }", "fn requires at least one parameter list")]
    [InlineData("fn[]() { 1 }", "empty implicit parameter list")]
    [InlineData("f [I64]", "implicit argument list must be adjacent to the callee; whitespace application is not supported")]
    [InlineData("fn [A : Type](a) { a }", "implicit fn parameter list must be adjacent to the callee; whitespace application is not supported")]
    public void Rejects(string source, string message) =>
        Assert.Equal(message, Assert.Throws<ExpandException>(() => Expander.ExpandExpr(source)).Message);

    // Unported forms name themselves rather than parsing into something else, and
    // are never an ordinary error: a conformance case expecting `error` must not
    // pass because a form is missing.
    [Theory]
    [InlineData("1 + 2", "not ported yet: the infix operator `+`")]
    [InlineData("ref (x)", "not ported yet: the `ref` form")]
    public void RejectsUnported(string source, string message) =>
        Assert.Equal(message, Assert.Throws<NotImplementedException>(() => Expander.ExpandExpr(source)).Message);
}
