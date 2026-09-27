using System.Reflection;
using Fun.Expand;
using Fun.Kernel;

namespace Fun.Compiler;

/// <summary>
/// The prelude (<c>std</c>): stage 2 of <c>std/</c>, which imports stage 1 as a
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
        var stage = new Stage(metas, ctx.Eval(term), type, expander.SyntaxExports);
        // Stage 1 defines the Syntax module every other stage re-exports: resolve the
        // whole declared interface here, before anything can read it lazily.
        if (std is null) Verify(stage);
        return stage;
    }

    // ---- verifying the declared interface ------------------------------------

    /// <summary>
    /// Resolves <see cref="PreludeAbi"/> against the loaded prelude, so a test can assert the
    /// declaration without running a program. Stage 1's own <see cref="Load"/> runs the same
    /// check as the stage is built; this is the half <c>dotnet test</c> can reach.
    /// </summary>
    public static void VerifyAbi() => Verify(SyntaxStage);

    /// <summary>
    /// Resolves every name <see cref="PreludeAbi"/> declares against the loaded stage,
    /// so a rename in the prelude fails here, at load, naming the member and the file it
    /// was looked for in - rather than lazily, at the first reflection use
    /// (<c>Reflection.Nominal</c>). Throws on the first name that does not resolve.
    /// </summary>
    private static void Verify(Stage stage)
    {
        var syntax = Nbe.Force(stage.Metas, Nbe.DotValue(stage.Value, PreludeAbi.Syntax));
        foreach (var name in Consts(typeof(PreludeAbi.Types.Syntax))) Resolve(stage, syntax, PreludeAbi.Syntax, name);
        foreach (var name in Consts(typeof(PreludeAbi.Types.Builtins))) Resolve(stage, stage.Value, null, name);
        foreach (var name in Consts(typeof(PreludeAbi.Builders.Syntax))) Resolve(stage, syntax, PreludeAbi.Syntax, name);
        foreach (var name in Consts(typeof(PreludeAbi.Builders.Builtins))) Resolve(stage, stage.Value, null, name);
        foreach (var nominal in typeof(PreludeAbi.Tags).GetNestedTypes())
        {
            var type = Resolve(stage, syntax, PreludeAbi.Syntax, nominal.Name);
            foreach (var tag in Consts(nominal))
                try { Nbe.DotValue(Nbe.Force(stage.Metas, type), tag); }
                catch (Exception e) { throw Missing($"{PreludeAbi.Syntax}.{nominal.Name}.{tag}", e); }
        }
    }

    /// <summary>The declared names of one group: its <c>const string</c> fields, in order.</summary>
    private static IEnumerable<string> Consts(Type group) =>
        group.GetFields(BindingFlags.Public | BindingFlags.Static).Where(f => f.IsLiteral)
            .Select(f => (string)f.GetRawConstantValue()!);

    private static Value Resolve(Stage stage, Value root, string? scope, string name)
    {
        try { return Nbe.Force(stage.Metas, Nbe.DotValue(Nbe.Force(stage.Metas, root), name)); }
        catch (Exception e) { throw Missing(scope is null ? name : scope + "." + name, e); }
    }

    /// <summary>A prelude that does not carry a declared name: the member and its file.</summary>
    private static InvalidOperationException Missing(string member, Exception inner) =>
        new($"PreludeAbi declares {member}, which the prelude {Stage1Path} does not define", inner);
}
