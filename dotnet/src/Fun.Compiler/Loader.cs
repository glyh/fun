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
/// <param name="prelude">
/// The unit name <c>stdlib</c> is served under: <c>"std"</c> for a program, and for the
/// prelude's own stages the stage below (<see cref="Prelude.Stage1Path"/>, or none).
/// </param>
/// <param name="macroMetas">
/// The metas macros are compiled and run with; the prelude passes its own so that a
/// macro it exports and the values it exports speak of the same metas.
/// </param>
public sealed class Loader(
    IReadOnlyDictionary<string, string> sources,
    string? prelude = Prelude.Path,
    MetaContext? macroMetas = null) : IMacroRuntime
{
    private readonly Dictionary<string, (Syntax Unit, UnitSyntax Syntax, Expander Expander)> _expanded = [];
    private readonly HashSet<string> _expanding = [];
    private readonly Dictionary<string, (Value Value, Value Type)> _loaded = [];
    private readonly HashSet<string> _active = [];

    public UnitSyntax LoadSyntax(string path) =>
        path == prelude ? Prelude.Of(path).Syntax : Expanded(path).Syntax;

    /// <summary>A unit expanded by its own expander, which this loader serves in turn.</summary>
    private (Syntax Unit, UnitSyntax Syntax, Expander Expander) Expanded(string path)
    {
        if (_expanded.TryGetValue(path, out var expanded)) return expanded;
        if (!sources.TryGetValue(path, out var source)) throw new FunException($"import not found: \"{path}\"");
        if (!_expanding.Add(path)) throw new FunException($"circular import: \"{path}\"");
        try
        {
            var expander = new Expander(new UnitRuntime(this));
            var unit = expander.ExpandUnit(Enforest.ParseUnit(source, path));
            return _expanded[path] = (unit, expander.SyntaxExports, expander);
        }
        finally
        {
            _expanding.Remove(path);
        }
    }

    // ---- the macro runtime ------------------------------------------------------

    /// <summary>
    /// The metas macros are compiled and run with. An application solves nothing its
    /// caller needs, and all of this loader's expansions spend from its one budget.
    /// They are seeded from the prelude's up front, not when a macro is first compiled:
    /// a macro the prelude exports is applied here without anything of this loader's
    /// being compiled first, and its value speaks of the prelude's metas.
    /// </summary>
    private readonly MetaContext _macroMetas = Seeded(macroMetas ?? new(), prelude);

    private static MetaContext Seeded(MetaContext metas, string? prelude)
    {
        if (prelude is not null) metas.SeedFrom(Prelude.Of(prelude).Metas);
        return metas;
    }

    private Context? _macroBase;

    /// <summary>
    /// Where a macro is compiled: the base context, nothing opened. The expander wraps a
    /// definition in the unit opens around it, so a body sees exactly its definition site.
    /// </summary>
    internal Context MacroBase => _macroBase ??= Elaborator.BaseContext(_macroMetas, prelude) with { Loader = this };

    /// <summary>Elaborates and evaluates a macro's definition or signature in <paramref name="site"/>.</summary>
    internal Value Compile(Context site, Syntax syntax)
    {
        var since = site.Metas.Count;
        var sink = new EffectSink();
        var (term, _) = Elaborator.Infer(site with { Sink = sink }, syntax);
        Elaborator.RequireHandledAtEntry(site, sink, since);
        return site.Eval(term);
    }

    /// <summary>A program is not a unit: it has no top level that advances.</summary>
    public void Advance(Binding expanded) =>
        throw new InvalidOperationException("only a compilation unit's top level advances");

    public Value CompileMacro(Syntax definition) => Compile(MacroBase, definition);

    public Value CompileSignature(Syntax signature) => Compile(MacroBase, signature);

    public Syntax ApplyExpr(string macro, MacroEntry entry, EquatableArray<Capture> args, MacroExpansion expansion) =>
        ApplyMacro(macro, entry.Value, [], args, expansion, output =>
            Reflection.OfPrelude.ReadExpr(output) ?? throw new FunException($"macro {macro} did not return syntax"));

    public EquatableArray<Binding> ApplyDecls(string macro, MacroEntry entry, EquatableArray<Capture> args, MacroExpansion expansion) =>
        ApplyMacro(macro, entry.Value, [], args, expansion, output =>
            Reflection.OfPrelude.ReadDeclOutput(output) ?? throw new FunException($"macro {macro} did not return declarations"));

    /// <summary>
    /// Applies <paramref name="macro"/> to its type binders' solutions, each as the
    /// reflected type it was solved to, then to each argument as the value of its kind,
    /// under the budget (M5); <paramref name="read"/> reads its output back.
    /// </summary>
    internal T ApplyMacro<T>(string macro, Value fn, IEnumerable<Value> types, EquatableArray<Capture> args, MacroExpansion expansion, Func<Value, T> read)
    {
        var r = Reflection.OfPrelude;
        var application = new MacroApplication(macro,
            v => r.ReflectExpr(expansion.ExpandBlock(r.ReadExpr(v) ?? throw new FunException($"expand_block in macro {macro}: not syntax"))),
            v => r.ReflectDecls(expansion.ExpandDecls(r.ReadDeclOutput(v) ?? throw new FunException($"expand_decls in macro {macro}: not declarations"))));
        return _macroMetas.Budget.MacroApplication(application, () =>
        {
            foreach (var type in types) fn = Nbe.Apply(_macroMetas, fn, r.ReflectType(type));
            foreach (var arg in args) fn = Nbe.Apply(_macroMetas, fn, r.ReflectCapture(arg));
            return read(fn);
        });
    }

    public (Value Value, Value Type) Load(string path, MetaContext metas)
    {
        // Elaborated once per process; the importer's metas are seeded from the prelude's.
        if (path == prelude) return (Prelude.Of(path).Value, Prelude.Of(path).Type);
        if (_loaded.TryGetValue(path, out var loaded)) return loaded;
        var (unit, _, expander) = Expanded(path);
        if (!_active.Add(path)) throw new FunException($"circular import: \"{path}\"");
        try
        {
            // Strict: the base context with nothing opened, sharing the importer's
            // metas so a meta the unit leaves unsolved stays meaningful to it.
            var ctx = Elaborator.BaseContext(metas, prelude) with { Loader = this, Expander = expander };
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
