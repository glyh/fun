using Fun.Kernel;

namespace Fun.Compiler;

public sealed partial record Context
{
    /// <summary>The units this elaboration may import. Null where no import can happen.</summary>
    public Loader? Loader { get; init; }
}

public static partial class Elaborator
{
    /// <summary>An import: the unit's value, transported, never its term (I5).</summary>
    private static (Term, Value) InferImport(Context ctx, Syntax.Import import)
    {
        var loader = ctx.Loader ?? throw new InvalidOperationException("an import outside an elaboration with a loader");
        var (value, type) = loader.Load(import.Path, ctx.Metas);
        return (new Term.Imported(value), type);
    }
}
