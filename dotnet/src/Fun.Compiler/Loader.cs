using Fun.Expand;
using Fun.Kernel;

namespace Fun.Compiler;

/// <summary>
/// The compilation units a program may import, by path: each one's expansion and
/// syntax exports, and its elaborated value and type once loaded. A unit elaborates
/// against the base context, so its meaning depends only on its own source plus what
/// it imports and opens; that is what makes caching it by path sound, and why a unit
/// imported twice is expanded and elaborated once.
/// </summary>
public sealed class Loader(IReadOnlyDictionary<string, string> sources) : IMacroRuntime
{
    private readonly Dictionary<string, (Syntax Unit, UnitSyntax Syntax)> _expanded = [];
    private readonly HashSet<string> _expanding = [];
    private readonly Dictionary<string, (Value Value, Value Type)> _loaded = [];
    private readonly HashSet<string> _active = [];

    public UnitSyntax LoadSyntax(string path) => path == Prelude.Path ? Prelude.Syntax : Expanded(path).Syntax;

    /// <summary>A unit expanded by its own expander, which this loader serves in turn.</summary>
    private (Syntax Unit, UnitSyntax Syntax) Expanded(string path)
    {
        if (_expanded.TryGetValue(path, out var expanded)) return expanded;
        if (!sources.TryGetValue(path, out var source)) throw new FunException($"import not found: \"{path}\"");
        if (!_expanding.Add(path)) throw new FunException($"circular import: \"{path}\"");
        try
        {
            var expander = new Expander(this);
            var unit = expander.Expand(Enforest.ParseUnit(source, path));
            return _expanded[path] = (unit, expander.SyntaxExports);
        }
        finally
        {
            _expanding.Remove(path);
        }
    }

    public (Value Value, Value Type) Load(string path, MetaContext metas)
    {
        // Elaborated once per process; the importer's metas are seeded from the prelude's.
        if (path == Prelude.Path) return Prelude.Unit;
        if (_loaded.TryGetValue(path, out var loaded)) return loaded;
        var (unit, _) = Expanded(path);
        if (!_active.Add(path)) throw new FunException($"circular import: \"{path}\"");
        try
        {
            // Strict: the base context with nothing opened, sharing the importer's
            // metas so a meta the unit leaves unsolved stays meaningful to it.
            var ctx = Elaborator.BaseContext(metas, preludeOpen: false) with { Loader = this };
            var since = metas.Count;
            var sink = new EffectSink();
            var (term, type) = Elaborator.Infer(ctx with { Sink = sink }, unit);
            // Only what this unit left open is its to settle; the importer's waits.
            Elaborator.RequireHandledAtEntry(ctx, sink, since);
            return _loaded[path] = (ctx.Eval(term), type);
        }
        finally
        {
            _active.Remove(path);
        }
    }
}
