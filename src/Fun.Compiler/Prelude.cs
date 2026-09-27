using Fun.Expand;
using Fun.Kernel;

namespace Fun.Compiler;

/// <summary>
/// The prelude (<c>std</c>): its units, lowest first - the bootstrap ABI, then the
/// library units, then the <c>std</c> unit a program imports. Each unit is elaborated
/// once per process against the units below it: the bootstrap against the builtins
/// alone, every library unit against the builtins with the units below it served to
/// its imports, and <c>std</c> against the whole library. The base context binds the
/// topmost unit as <c>stdlib</c> (glossary: Base context, Prelude; bound, not opened).
/// </summary>
public static class Prelude
{
    public const string Path = "std";

    /// <summary>
    /// The ABI unit: <c>Syntax</c>, its reflection builders, and the three nominals the
    /// compiler reads by name (<see cref="Reflection"/>).
    /// </summary>
    public const string BootstrapPath = "std/bootstrap";

    public const string Binding = "stdlib";

    /// <summary>The prelude's units, lowest first. Each is elaborated against the ones
    /// before it, and <see cref="Path"/> is the one a program imports.</summary>
    private static readonly string[] Order = [BootstrapPath, "std/list", "std/lib", "std/type", Path];

    private static readonly Dictionary<string, int> Index =
        Order.Select((path, i) => (path, i)).ToDictionary(x => x.path, x => x.i);

    /// <summary>Each unit's source file under <c>std/</c>.</summary>
    private static string File(string path) =>
        path == Path ? "stage2.fun" : path["std/".Length..] + ".fun";

    internal sealed record Stage(MetaContext Metas, Value Value, Value Type, UnitSyntax Syntax);

    private static readonly Lazy<Stage>[] Stages =
        [.. Order.Select((path, i) => new Lazy<Stage>(() => Load(path, [.. Order.Take(i)])))];

    /// <summary>The stage a loader serving <paramref name="path"/> hands out.</summary>
    internal static Stage Of(string path) =>
        Index.TryGetValue(path, out var i) ? Stages[i].Value
            : throw new InvalidOperationException($"not a prelude unit: {path}");

    /// <summary>
    /// What reflection reads the <c>Syntax</c> module off. It is declared in the
    /// bootstrap, which every later unit only re-exports, and the later stages' metas
    /// extend the bootstrap's - so the bootstrap is the prelude's <c>Syntax</c> module,
    /// and reading it does not wait on a unit whose own body quotes while it is still
    /// being elaborated.
    /// </summary>
    internal static Stage SyntaxStage => Of(BootstrapPath);

    public static string Source(string file)
    {
        using var stream = typeof(Prelude).Assembly.GetManifestResourceStream($"std/{file}")
            ?? throw new InvalidOperationException($"the prelude source std/{file} is not embedded");
        return new StreamReader(stream).ReadToEnd();
    }

    /// <summary>
    /// Elaborates one unit as a compilation unit. <paramref name="below"/> are the
    /// prelude units its own imports see - none for the bootstrap, every lower unit for
    /// a library unit - and one meta context serves its expansion, its macros and its
    /// elaboration, so the metas in the values it exports mean the same wherever they
    /// are later seeded.
    /// </summary>
    private static Stage Load(string path, IReadOnlyList<string> below)
    {
        var file = File(path);
        var metas = new MetaContext();
        var loader = new Loader(new Dictionary<string, string>(), below, metas);
        var expander = new Expander(new UnitRuntime(loader));
        var unit = expander.ExpandUnit(Enforest.ParseUnit(Source(file), $"std/{file}"));
        var ctx = loader.MacroBase with { Expander = expander };
        var sink = new EffectSink();
        var (term, type) = Elaborator.Infer(ctx with { Sink = sink }, unit);
        Elaborator.RequireHandledAtEntry(ctx, sink, since: 0);
        return new Stage(metas, ctx.Eval(term), type, expander.SyntaxExports);
    }
}
