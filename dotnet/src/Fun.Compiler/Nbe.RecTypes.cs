using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Nbe
{
    private abstract partial record Kont
    {
        /// <summary>An occurrence's captures then arguments are evaluated (as a tuple); build it.</summary>
        public sealed record RecursiveOccurrenceOf(RecordDecl Decl, int CaptureCount) : Kont;
    }

    private static Value RecursiveOccurrence(Kont.RecursiveOccurrenceOf f, Value.VProd parts) =>
        new Value.VRecursiveOccurrence(f.Decl, [.. parts.Items.Take(f.CaptureCount)], [.. parts.Items.Skip(f.CaptureCount)]);

    /// <summary>
    /// A recursive occurrence as its struct type: the finished body evaluated in the
    /// declaring environment with the occurrence's captures at the levels they were
    /// taken from, applied to its arguments - one instance per captures. An
    /// occurrence inside its own body (not yet finished) stays as it is; anything
    /// else is returned forced.
    /// </summary>
    public static Value Unfold(MetaContext mc, Value value)
    {
        value = Force(mc, value);
        if (value is not Value.VRecursiveOccurrence { Decl: { IsFinished: true } decl } occurrence) return value;
        var env = decl.Environment!;
        for (var i = 0; i < decl.Levels.Length; i++)
            env = env.Replace(LevelToIndex(env.Count, decl.Levels[i]), occurrence.Captures[i]);
        return Force(mc, occurrence.Args.Aggregate(Eval(mc, env, decl.Body!), (f, a) => Apply(mc, f, a)));
    }

    private static Term QuoteRecursiveOccurrence(MetaContext mc, int width, Value.VRecursiveOccurrence o) =>
        new Term.RecursiveOccurrence(o.Decl, [.. o.Captures.Select(c => Quote(mc, width, c))], [.. o.Args.Select(a => Quote(mc, width, a))]);
}
