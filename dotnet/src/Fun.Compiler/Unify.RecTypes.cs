using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Unify
{
    /// <summary>
    /// Occurrences are equal by identity - the same declaration, its captures and
    /// arguments agreeing; an occurrence meets anything else as its unfolding.
    /// </summary>
    private static void RecursiveOccurrences(MetaContext mc, int width, Value left, Value right)
    {
        if (left is Value.VRecursiveOccurrence a && right is Value.VRecursiveOccurrence b)
        {
            if (!ReferenceEquals(a.Decl, b.Decl))
                throw new UnifyException($"recursive occurrence {a.Decl.Name} vs recursive occurrence {b.Decl.Name}");
            Pairwise(mc, width, a.Captures, b.Captures);
            Pairwise(mc, width, a.Args, b.Args);
            return;
        }

        var (unfoldedLeft, unfoldedRight) = (Nbe.Unfold(mc, left), Nbe.Unfold(mc, right));
        if (unfoldedLeft is Value.VRecursiveOccurrence || unfoldedRight is Value.VRecursiveOccurrence)
            throw new UnifyException($"cannot unify {left.GetType().Name} with {right.GetType().Name}");
        Values(mc, width, unfoldedLeft, unfoldedRight);
    }
}
