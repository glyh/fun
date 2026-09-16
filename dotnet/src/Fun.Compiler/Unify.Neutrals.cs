using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Unify
{
    /// <summary>Two stuck computations are equal when their heads are and their frames agree one by one.</summary>
    private static void Neutrals(MetaContext mc, int width, Value.VNeutral a, Value.VNeutral b)
    {
        if (a.Head != b.Head || a.Frames.Length != b.Frames.Length)
            throw new UnifyException("stuck computations with different heads");
        for (var i = 0; i < a.Frames.Length; i++)
        {
            switch (a.Frames[i], b.Frames[i])
            {
                case (Frame.FApp x, Frame.FApp y): Values(mc, width, x.Arg, y.Arg); break;
                case (Frame.FProj x, Frame.FProj y) when x.Index == y.Index: break;
                case (Frame.FDot x, Frame.FDot y) when x.Name == y.Name: break;
                case (Frame.FApp or Frame.FProj or Frame.FDot, Frame.FApp or Frame.FProj or Frame.FDot):
                    throw new UnifyException("stuck computations with different eliminations");
                default:
                    throw new NotImplementedException($"not ported yet: unifying stuck {a.Frames[i].GetType().Name} frames");
            }
        }
    }
}
