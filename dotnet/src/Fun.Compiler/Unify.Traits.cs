using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Unify
{
    /// <summary>
    /// Traits are nominal: one declaration is one trait. Two dictionary types agree
    /// when they are one trait at agreeing arguments with agreeing operations.
    /// </summary>
    private static void UnifyTraits(MetaContext mc, int width, Value left, Value right)
    {
        switch (left, right)
        {
            case (Value.VTrait a, Value.VTrait b) when ReferenceEquals(a.Decl, b.Decl):
                return;
            case (Value.VTraitDict a, Value.VTraitDict b) when ReferenceEquals(a.Decl, b.Decl):
                Pairwise(mc, width, a.Args, b.Args);
                if (a.Operations.Length != b.Operations.Length) throw new UnifyException("trait dictionaries with different operations");
                for (var i = 0; i < a.Operations.Length; i++)
                {
                    if (a.Operations[i].Name != b.Operations[i].Name) throw new UnifyException("trait dictionaries with different operations");
                    Values(mc, width, a.Operations[i].Type, b.Operations[i].Type);
                }
                return;
            default:
                throw new UnifyException($"cannot unify {DescribeTrait(left)} with {DescribeTrait(right)}");
        }
    }

    private static string DescribeTrait(Value v) => v switch
    {
        Value.VTrait t => $"trait {t.Decl.Name}",
        Value.VTraitDict d => $"trait dictionary {d.Decl.Name}",
        _ => v.GetType().Name,
    };

    /// <summary>
    /// Module types agree on their public impls as on their fields: a partial side
    /// (a signature's instance) needs each impl it names in the other, at an
    /// agreeing dictionary type; two whole modules need the same impls in order.
    /// </summary>
    private static void ModuleImpls(MetaContext mc, int width, Value.VModule a, Value.VModule b)
    {
        var ia = a.Entries.OfType<ModuleEntry.Impl>().Where(i => i.Kind == MemberKind.Public).ToList();
        var ib = b.Entries.OfType<ModuleEntry.Impl>().Where(i => i.Kind == MemberKind.Public).ToList();
        if (!a.Partial && !b.Partial)
        {
            if (ia.Count != ib.Count) throw new UnifyException("modules with different impls");
            for (var i = 0; i < ia.Count; i++) Values(mc, width, ia[i].DictType, ib[i].DictType);
            return;
        }
        var (required, available) = a.Partial ? (ia, ib) : (ib, ia);
        foreach (var impl in required)
        {
            var other = available.LastOrDefault(o => o.Name == impl.Name)
                ?? throw new UnifyException($"no impl `{impl.Name ?? DescribeTrait(impl.DictType)}`");
            Values(mc, width, impl.DictType, other.DictType);
        }
    }

    /// <summary>The values a dictionary type holds, for the occurs check.</summary>
    private static IEnumerable<Value> TraitContents(Value.VTraitDict dict) =>
        [.. dict.Args, .. dict.Operations.Select(o => o.Type)];
}
