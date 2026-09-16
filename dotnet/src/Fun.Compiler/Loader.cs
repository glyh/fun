using Fun.Kernel;

namespace Fun.Compiler;

/// <summary>
/// The compilation units a program may import, by path, and each one's
/// elaborated value and type once loaded. A unit elaborates against the base
/// context, so its meaning depends only on its own source plus what it imports
/// and opens; that is what makes caching it by path sound, and why a unit
/// imported twice is elaborated once.
/// </summary>
public sealed class Loader(IReadOnlyDictionary<string, string> sources)
{
    private readonly Dictionary<string, (Value Value, Value Type)> _loaded = [];
    private readonly HashSet<string> _active = [];

    public (Value Value, Value Type) Load(string path, MetaContext metas)
    {
        if (path == "std") throw new NotImplementedException("not ported yet: the prelude (`import \"std\"`)");
        if (_loaded.TryGetValue(path, out var loaded)) return loaded;
        if (!sources.TryGetValue(path, out var source)) throw new FunException($"import not found: \"{path}\"");
        if (!_active.Add(path)) throw new FunException($"circular import: \"{path}\"");
        try
        {
            var unit = new Fun.Expand.Expander().Expand(Fun.Expand.Enforest.ParseUnit(source, path));
            // Strict: the base context with nothing opened, sharing the importer's
            // metas so a meta the unit leaves unsolved stays meaningful to it.
            var ctx = Elaborator.BaseContext(metas, preludeOpen: false) with { Loader = this };
            var (term, type) = Elaborator.Infer(ctx, unit);
            return _loaded[path] = (ctx.Eval(term), type);
        }
        finally
        {
            _active.Remove(path);
        }
    }
}
