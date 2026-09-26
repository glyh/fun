using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Unify
{
    /// <summary>
    /// Lazy delta: two deferred calls of the same fixpoint on convertible arguments
    /// are equal without unfolding either. Arguments are compared by conversion, so
    /// a failed shortcut solves no meta; unification then unfolds both.
    /// </summary>
    private static bool SameDeferredCall(MetaContext mc, int width, Value left, Value right) =>
        left is Value.VGlued a && right is Value.VGlued b
        && a.Fix.IsSame(b.Fix) && Nbe.Convertible(mc, width, a.Arg, b.Arg);

    /// <summary>Two members of recursive groups: the same member of groups whose bodies unify.</summary>
    private static void FixBodies(MetaContext mc, int width, Value.VFix a, Value.VFix b)
    {
        if (a.Index != b.Index || a.Members.Length != b.Members.Length)
            throw new UnifyException("different recursive definitions");
        var count = a.Members.Length;
        var inner = width + count;
        Environment Under(Value.VFix fix) =>
            Enumerable.Range(0, count).Aggregate(fix.Environment, (e, i) => e.Push(new Value.VVar(width + i, [])));
        for (var i = 0; i < count; i++)
            Values(mc, inner, Nbe.Eval(mc, Under(a), a.Members[i].Body), Nbe.Eval(mc, Under(b), b.Members[i].Body));
    }
}
