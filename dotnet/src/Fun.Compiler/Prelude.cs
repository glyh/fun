using Fun.Expand;
using Fun.Kernel;

namespace Fun.Compiler;

/// <summary>
/// The prelude (<c>std</c>): stage 2 of <c>dotnet/std</c>, which imports stage 1 as a
/// unit of its own (<see cref="Stage1Path"/>) and re-exports it. Each stage is
/// elaborated once per process - stage 1 against the builtins alone, stage 2 against
/// the builtins with stage 1 bound as <c>stdlib</c> - and the base context binds
/// stage 2 as <c>stdlib</c> (glossary: Base context, Prelude; bound, not opened).
/// </summary>
public static class Prelude
{
    public const string Path = "std";

    /// <summary>
    /// Stage 1's own unit name. <c>"std"</c> means stage 2 and nothing else, so stage 2
    /// reaches stage 1 by this name instead (port-only: the prototype has no unit loader
    /// and so no collision).
    /// </summary>
    public const string Stage1Path = "std/stage1";

    public const string Binding = "stdlib";

    internal sealed record Stage(MetaContext Metas, Value Value, Value Type, UnitSyntax Syntax);

    private static readonly Lazy<Stage> Stage1 = new(() => Load("stage1.fun", std: null));
    private static readonly Lazy<Stage> Stage2 = new(() => Load("stage2.fun", std: Stage1Path));

    /// <summary>The stage a loader serving <paramref name="path"/> as its prelude hands out.</summary>
    internal static Stage Of(string path) => path == Stage1Path ? Stage1.Value : Stage2.Value;

    /// <summary>
    /// What reflection reads the <c>Syntax</c> module off. It is declared in stage 1,
    /// which stage 2 only re-exports, and stage 2's metas extend stage 1's - so stage 1
    /// is the prelude's <c>Syntax</c> module, and reading it does not wait on stage 2,
    /// whose own body quotes while it is still being elaborated.
    /// </summary>
    internal static Stage SyntaxStage => Stage1.Value;

    public static string Source(string file)
    {
        using var stream = typeof(Prelude).Assembly.GetManifestResourceStream($"std/{file}")
            ?? throw new InvalidOperationException($"the prelude source std/{file} is not embedded");
        return new StreamReader(stream).ReadToEnd();
    }

    /// <summary>
    /// Elaborates one stage as a compilation unit. <paramref name="std"/> is the prelude
    /// its own imports see - none for stage 1, stage 1 for stage 2 - and one meta context
    /// serves its expansion, its macros and its elaboration, so the metas in the values
    /// it exports mean the same wherever they are later seeded.
    /// </summary>
    private static Stage Load(string file, string? std)
    {
        var metas = new MetaContext();
        var loader = new Loader(new Dictionary<string, string>(), std, metas);
        var expander = new Expander(new UnitRuntime(loader));
        var unit = expander.ExpandUnit(Enforest.ParseUnit(Source(file), $"std/{file}"));
        var ctx = loader.MacroBase with { Expander = expander };
        var sink = new EffectSink();
        var (term, type) = Elaborator.Infer(ctx with { Sink = sink }, unit);
        Elaborator.RequireHandledAtEntry(ctx, sink, since: 0);
        return new Stage(metas, ctx.Eval(term), type, expander.SyntaxExports);
    }
}
