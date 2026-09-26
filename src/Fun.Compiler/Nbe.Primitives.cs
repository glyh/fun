using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Nbe
{
    /// <summary>A stuck application headed by a primitive reduces once its reducer can; any other stays stuck.</summary>
    private static Value ReduceNeutral(MetaContext mc, Value.VNeutral neutral) =>
        neutral.Head is Head.HPrim prim && Primitives.Reduce(mc, prim.Name, neutral.Frames) is { } reduced
            ? reduced
            : neutral;
}
