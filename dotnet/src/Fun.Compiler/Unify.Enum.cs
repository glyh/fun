using System.Collections.Immutable;
using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Unify
{
    /// <summary>
    /// Two nominals are one type when they come from one declaration and their
    /// captures are convertible (E11); two constructed values agree when their
    /// tags, payloads and nominals do.
    /// </summary>
    private static void UnifyNominals(MetaContext mc, int width, Value left, Value right)
    {
        switch (left, right)
        {
            case (Value.VNominal a, Value.VNominal b) when ReferenceEquals(a.Decl, b.Decl):
                Pairwise(mc, width, a.Captures, b.Captures);
                return;
            case (Value.VCon a, Value.VCon b) when a.Name == b.Name:
                Values(mc, width, a.Nominal, b.Nominal);
                Pairwise(mc, width, a.Args, b.Args);
                return;
            default:
                throw new UnifyException($"cannot unify {Describe(left)} with {Describe(right)}");
        }
    }

    private static string Describe(Value v) => v switch
    {
        Value.VNominal n => n.Decl.ToString(),
        Value.VCon c => c.Name,
        _ => v.GetType().Name,
    };

    /// <summary>
    /// Reads <paramref name="value"/>, a value at <paramref name="width"/> entries,
    /// back as a term over just <paramref name="levels"/>: the <c>i</c>th level
    /// becomes entry <c>i</c>. Any other variable is an escape.
    /// </summary>
    internal static Term CloseOver(MetaContext mc, int width, EquatableArray<int> levels, Value value) =>
        Rename(mc, -1, new Renaming(levels.Length, width, levels.Select((l, i) => (l, i)).ToImmutableDictionary(p => p.l, p => p.i)), value);
}
