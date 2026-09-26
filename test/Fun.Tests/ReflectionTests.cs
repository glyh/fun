using Fun.Compiler;
using Fun.Expand;
using Fun.Kernel;

namespace Fun.Tests;

/// <summary>
/// Reflection round trips (M1): reading back what was reflected is the identity
/// on every field the reflection grammar carries.
/// </summary>
public class ReflectionTests
{
    private static readonly Reflection R = Reflection.OfPrelude;

    /// <summary>
    /// What the reflection grammar has no slot for, cleared on both sides: a path
    /// member's span, and the roles an open's region holds (recomputed by expansion).
    /// </summary>
    private static readonly SyntaxMapper Normal = new()
    {
        Id = id => id with { Span = SourceSpan.Synthetic },
        Token = token => token with { Span = SourceSpan.Synthetic },
        Form = form => form switch
        {
            Syntax.Open o => o with { RolesInRegion = [], Span = SourceSpan.Synthetic },
            _ => form with { Span = SourceSpan.Synthetic },
        },
        Binding = binding => binding is Binding.Open o ? o with { RolesInRegion = [] } : binding,
    };

    private static Syntax Expanded(string source) =>
        Expander.ExpandExpr(source, new Loader(new Dictionary<string, string>()), openPrelude: true);

    [Theory]
    [InlineData("42")]
    [InlineData("(fn(x : I64) { x })(7)")]
    [InlineData("{ x : I64 = 5; y = 'c'; z = \"s\"; (x, y, z).0 }")]
    [InlineData("fn[A : Type](a : A) : A { a }")]
    [InlineData("{ rec f = fn(n : I64) : I64 { f(n) } and g = fn(n : I64) : I64 { g(n) }; f }")]
    [InlineData("{ M = module { pub x = 1; y = 2; pub T = enum { A, B(I64) } }; open M; x }")]
    [InlineData("{ P = struct { x : I64; pub method get() : I64 { self.x } }; P{x = 1}.x }")]
    [InlineData("{ S = sig { x : I64 }; S }")]
    [InlineData("{ C = enum { Red, Green(I64, I64) }; open C; match (Red) { Red | Green(_, _) => 1, _ => 2 } }")]
    [InlineData("{ effect E = sig { op : I64 -> I64 }; f : I64 ->{E} I64 = fn(x) { perform E.op(x) }; match (f(1)) { v => v, effect E.op n => resume(n) } }")]
    [InlineData("{ r = ref(1); r <- deref(r); r }")]
    [InlineData("fn(f : I64 ->{_} I64) ~> I64 { f(1) }")]
    [InlineData("{ trait Size(A) = sig { size : A -> I64 }; impl Size(I64) = module { size = fn(x) { 8 } }; f : [A : Size] -> A -> I64 = fn[A : Type](x) { Size.size(x) }; f(3) }")]
    [InlineData("{ P = struct { x : I64 }; fn(t : Type) { match (t) { I64 => 1, struct { x : _; _ } => 2, _ => 3 } } }")]
    [InlineData("{ M = module { pub pattern Two(a, b) = (a, b) }; 1 }")]
    public void AnExpandedProgramRoundTrips(string source)
    {
        var program = Expanded(source);
        Assert.Equal(program.Map(Normal), R.ReadExpr(R.ReflectExpr(program))?.Map(Normal));
    }

    /// <summary>
    /// A parameter's trait-bound paths are part of the reflection grammar: what a
    /// macro puts in MkParam's bound list reads back. The elaborator reads source
    /// bounds from the type (a TraitBoundSet), so no program can observe this field.
    /// </summary>
    [Fact]
    public void AParameterWithTraitBoundsRoundTrips()
    {
        var param = new Param(new Id("A", SourceSpan.Synthetic), null, Explicitness.Implicit,
            [new Syntax.Var(new Id("Eq", SourceSpan.Synthetic))]);
        var lambda = new Syntax.Lam(param, new Syntax.Atom(Atom.Unit.Instance, SourceSpan.Synthetic), SourceSpan.Synthetic);
        Assert.Equal(lambda.Map(Normal), R.ReadExpr(R.ReflectExpr(lambda))?.Map(Normal));
    }

    /// <summary>An operator macro receives its whole use: operator, fixity, operands, spans and unit.</summary>
    [Fact]
    public void AnOperatorUseRoundTrips()
    {
        var span = SourceSpan.Make(3, 8, "t.fun", 1, 3, 1, 8);
        var use = new Syntax.OperatorUse(new Id("~", span, ScopeSet.Of([4])), Fixity.Infix,
            [new Syntax.Atom(new Atom.I64(1), span), new Syntax.Atom(new Atom.I64(2), span)],
            SourceSpan.Make(0, 2, "t.fun", 1, 0, 1, 2), "u", span);
        Assert.Equal(use.Map(Normal), R.ReadExpr(R.ReflectExpr(use))?.Map(Normal));
    }

    [Fact]
    public void AnUnreadBlockRoundTripsWithItsTokensAndScopes()
    {
        var block = new Syntax.Block(Reader.Read("{ x = $y; (a, b) }").ToArray() is var ts
            ? [.. ts.Select(t => t.AddScope(ScopeSet.Of([3, 7])))] : [], SourceSpan.Make(0, 10, "f", 1, 0, 1, 10));
        Assert.Equal(block, R.ReadExpr(R.ReflectExpr(block)));
    }

    [Fact]
    public void SpansAreCarried()
    {
        var span = SourceSpan.Make(4, 9, "file.fun", 2, 1, 2, 6);
        var atom = new Syntax.Atom(new Atom.I64(3), span);
        Assert.Equal(atom, R.ReadExpr(R.ReflectExpr(atom)));
    }

    [Fact]
    public void RolesRulesAndInstantiationsRoundTrip()
    {
        var hole = new RulePart.Hole("x", HoleKind.Expr, SourceSpan.Synthetic);
        var literal = new RulePart.Literal(new TokenTree.Leaf(new Token(new TokenKind.Ident("twice"), SourceSpan.Synthetic, ScopeSet.Of([2]))));
        var rule = new Rule([literal, new RulePart.Group(Delimiter.Paren, [hole], SourceSpan.Synthetic)],
            new Replacement.Expr(new Syntax.Var(new Id("$x", SourceSpan.Synthetic))), SourceSpan.Synthetic);
        var order = new Order("g@1", "g", Assoc.Left, false, [new Order("h@2", "h", Assoc.None, true, [], [])], []);
        var role = new Role(Fixity.Prefix, order, new RoleMeaning.Rules(FormKind.Expr, [rule]), SourceSpan.Synthetic, "u");
        var inst = new Instantiation(new Id("twice", SourceSpan.Synthetic), rule,
            [("x", new Capture.Expr(new Syntax.Atom(new Atom.I64(1), SourceSpan.Synthetic))),
             ("t", new Capture.Tokens([new TokenTree.Leaf(new Token(TokenKind.Comma, SourceSpan.Synthetic))]))], null);
        Syntax form = new Syntax.SyntaxDef(new Id("twice", SourceSpan.Synthetic), role,
            new Syntax.Instantiate(inst, SourceSpan.Synthetic), SourceSpan.Synthetic);
        Assert.Equal(form, R.ReadExpr(R.ReflectExpr(form)));

        Binding decl = new Binding.SyntaxDecl(new Id("twice", SourceSpan.Synthetic), role, true);
        Assert.Equal(decl, R.ReadDecl(R.ReflectDecl(decl)));
    }

    [Fact]
    public void AResolvedNameNeedsItsCertificate()
    {
        var id = new Id("x#3", SourceSpan.Synthetic, ScopeSet.Of([1]));
        Assert.Equal(id, R.ReadId(R.ReflectId(id)));

        // A macro spelling a resolved name on an id it built carries no certificate.
        var forged = new Value.VRecord(R.IdType,
        [
            ("name", new Value.VAtom(new Atom.Str("x#3"))),
            ("span", R.ReflectId(id) is Value.VRecord r ? r.Fields[1].Value : throw new InvalidOperationException()),
            ("scope", new Value.VAtom(new Atom.Scopes(ScopeSet.Of([1]), null))),
        ]);
        Assert.Null(R.ReadId(forged));
    }

    [Fact]
    public void AValueThatIsNotReflectionReadsBackAsNull()
    {
        Assert.Null(R.ReadExpr(new Value.VAtom(new Atom.I64(1))));
        Assert.Null(R.ReadDecl(R.ReflectExpr(new Syntax.Atom(Atom.Unit.Instance, SourceSpan.Synthetic))));
    }
}
