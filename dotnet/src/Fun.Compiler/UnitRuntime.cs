using Fun.Expand;
using Fun.Kernel;

namespace Fun.Compiler;

/// <summary>
/// The macro runtime a compilation unit is expanded with: its loader's, except that a
/// macro compiles in the unit's context as of its definition (M3). Each top-level
/// binding is elaborated into that context as soon as it is expanded, starting from
/// <paramref name="start"/> (the base context with nothing opened; the builtins alone for
/// the prelude, which the base context binds), so the unit sees only what it binds and opens.
/// </summary>
public sealed class UnitRuntime(Loader loader, Func<Context> start) : IMacroRuntime
{
    public UnitRuntime(Loader loader) : this(loader, () => loader.MacroBase) { }

    private Context? _unit;

    private Context Unit => _unit ??= start();

    public UnitSyntax LoadSyntax(string path) => loader.LoadSyntax(path);

    public void Advance(Binding expanded) => _unit = Elaborator.AdvanceUnit(Unit with { Sink = new EffectSink() }, expanded);

    public Value CompileMacro(Syntax definition) => loader.Compile(Unit, definition);

    public Value CompileSignature(Syntax signature) => loader.Compile(Unit, signature);

    public Syntax ApplyExpr(string macro, MacroEntry entry, EquatableArray<Capture> args, MacroExpansion expansion, SourceSpan? site) =>
        loader.ApplyExpr(macro, entry, args, expansion, site);

    public EquatableArray<Binding> ApplyDecls(string macro, MacroEntry entry, EquatableArray<Capture> args, MacroExpansion expansion, SourceSpan? site) =>
        loader.ApplyDecls(macro, entry, args, expansion, site);
}
