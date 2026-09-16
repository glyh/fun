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
