using Fun.Expand;
using Fun.Kernel;

namespace Fun.Compiler;

/// <summary>
/// The prelude (`std`): stage 1 of <c>dotnet/std</c>, elaborated once per process
/// against the builtins alone, and bound in the base context as <c>stdlib</c>
/// (glossary: Base context, Prelude). Stage 2 is not ported.
/// </summary>
public static class Prelude
{
    public const string Path = "std";
    public const string Binding = "stdlib";

    private sealed record Stage(MetaContext Metas, Value Value, Value Type, UnitSyntax Syntax);

    private static readonly Lazy<Stage> Stage1 = new(LoadStage1);

    /// <summary>The metas the prelude was elaborated with; every base context is seeded from them.</summary>
    internal static MetaContext Metas => Stage1.Value.Metas;

    public static (Value Value, Value Type) Unit => (Stage1.Value.Value, Stage1.Value.Type);

    /// <summary>The prelude's syntax exports: the roles an open of <c>import "std"</c> brings.</summary>
    public static UnitSyntax Syntax => Stage1.Value.Syntax;

    private static readonly Lazy<HashSet<string>> Stage2 = new(() => PublicNames(Source("stage2.fun")));

    /// <summary>
    /// Every name stage 2 publishes - values, operators, order groups, traits, macros,
    /// syntax forms - read off its source. Stage 2 is not ported, so a name or role in
    /// this set that nothing supplies is "not ported yet"; any other is really missing.
    /// </summary>
    public static IReadOnlySet<string> Stage2Names => Stage2.Value;

    /// <summary>Whether any token of <paramref name="sources"/> spells a name stage 2 publishes.</summary>
    public static bool SpellsStage2Name(IEnumerable<string> sources) =>
        sources.Any(s => Reader.Scan(s).Any(t => t.Kind switch
        {
            TokenKind.Ident i => Stage2Names.Contains(i.Name),
            TokenKind.Operator o => Stage2Names.Contains(o.Spelling),
            _ => false,
        }));

    /// <summary>
    /// The names a unit's top-level <c>pub</c> items bind: <c>pub x</c>, <c>pub (op)</c>,
    /// and the name after <c>order</c>, <c>infix</c>, <c>prefix</c>, <c>syntax</c>,
    /// <c>trait</c> or <c>macro</c>. <c>pub impl</c> binds none.
    /// </summary>
    private static HashSet<string> PublicNames(string source)
    {
        var items = Reader.Read(source);
        var names = new HashSet<string>(StringComparer.Ordinal);
        for (var i = 0; i + 1 < items.Length; i++)
        {
            if (items[i] is not TokenTree.Leaf { Token.Kind: var pub } || pub != TokenKind.Pub) continue;
            var j = i + 1;
            while (j < items.Length && items[j] is TokenTree.Leaf { Token.Kind: var k }
                   && (k is TokenKind.Ident { Name: "order" or "infix" or "prefix" or "syntax" } || k == TokenKind.Trait || k == TokenKind.Macro))
                j++;
            var name = j < items.Length ? items[j] switch
            {
                TokenTree.Leaf { Token.Kind: TokenKind.Ident id } => id.Name,
                TokenTree.Group { Delimiter: Delimiter.Paren, Items: [TokenTree.Leaf { Token.Kind: TokenKind.Operator op }] } => op.Spelling,
                TokenTree.Group { Delimiter: Delimiter.Paren, Items: [TokenTree.Leaf { Token.Kind: TokenKind.Ident id }] } => id.Name,
                _ => null,
            } : null;
            if (name is not null) names.Add(name);
        }
        return names;
    }

    public static string Source(string file)
    {
        using var stream = typeof(Prelude).Assembly.GetManifestResourceStream($"std/{file}")
            ?? throw new InvalidOperationException($"the prelude source std/{file} is not embedded");
        return new StreamReader(stream).ReadToEnd();
    }

    private static Stage LoadStage1()
    {
        // Stage 1 imports nothing, so its expander's loader has no units to serve.
        var expander = new Expander(new Loader(new Dictionary<string, string>()));
        var unit = expander.Expand(Enforest.ParseUnit(Source("stage1.fun"), "std/stage1.fun"));
        var metas = new MetaContext();
        var ctx = Elaborator.BuiltinContext(metas, preludeOpen: false);
        var sink = new EffectSink();
        var (term, type) = Elaborator.Infer(ctx with { Sink = sink }, unit);
        Elaborator.RequireHandledAtEntry(ctx, sink, since: 0);
        return new Stage(metas, ctx.Eval(term), type, expander.SyntaxExports);
    }
}
