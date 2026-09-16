using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Unify
{
    /// <summary>Two signatures agree when they describe the same (fresh) module alike.</summary>
    private static void Signatures(MetaContext mc, int width, Value.VSig a, Value.VSig b)
    {
        var fresh = new Value.VVar(width, []);
        Values(mc, width + 1, Nbe.ApplyClosure(mc, a.Body, fresh), Nbe.ApplyClosure(mc, b.Body, fresh));
    }

    /// <summary>
    /// Module types agree on their public members. A partial side (a signature's
    /// instance) needs only its own members present, with agreeing types, in
    /// the other side: width subtyping.
    /// </summary>
    private static void Modules(MetaContext mc, int width, Value.VModule a, Value.VModule b)
    {
        var va = Members(a.Entries, k => k != MemberKind.Private);
        var vb = Members(b.Entries, k => k != MemberKind.Private);
        if (!a.Partial && !b.Partial && va.Count != vb.Count)
            throw new UnifyException("modules with different members");
        var (required, available) = (a.Partial, b.Partial) switch
        {
            (false, false) or (true, false) => (va, vb),
            (false, true) => (vb, va),
            _ => va.Count <= vb.Count ? (va, vb) : (vb, va),
        };
        foreach (var member in required)
        {
            var other = available.LastOrDefault(o => o.Name == member.Name && o.Kind == member.Kind)
                ?? throw new UnifyException($"no member `{member.Name}`");
            Values(mc, width, member.Value, other.Value);
        }
    }

    /// <summary>
    /// Struct types agree member for member, in order; a partial one needs only
    /// its members present in the other, where a field and a public binding of a
    /// name stand for each other.
    /// </summary>
    private static void Structs(MetaContext mc, int width, Value.VStruct a, Value.VStruct b)
    {
        static bool Shown(MemberKind k) => k is not (MemberKind.Private or MemberKind.PrivateMethod);
        var va = Members(a.Entries, Shown);
        var vb = Members(b.Entries, Shown);

        if (!a.Partial && !b.Partial)
        {
            if (va.Count != vb.Count) throw new UnifyException("structs with different members");
            for (var i = 0; i < va.Count; i++)
            {
                if (va[i].Name != vb[i].Name || va[i].Kind != vb[i].Kind) throw new UnifyException("structs with different members");
                Values(mc, width, va[i].Value, vb[i].Value);
            }
            return;
        }

        var (small, large) = va.Count <= vb.Count ? (va, vb) : (vb, va);
        foreach (var member in small)
        {
            var other = large.FirstOrDefault(o => o.Name == member.Name && (o.Kind == member.Kind || FieldOrPublic(o.Kind) && FieldOrPublic(member.Kind)))
                ?? throw new UnifyException($"no member `{member.Name}`");
            Values(mc, width, member.Value, other.Value);
        }
    }

    private static bool FieldOrPublic(MemberKind kind) => kind is MemberKind.Field or MemberKind.Public;

    private static void Records(MetaContext mc, int width, Value.VRecord a, Value.VRecord b)
    {
        Values(mc, width, a.Type, b.Type);
        if (a.Fields.Length != b.Fields.Length) throw new UnifyException("records with different fields");
        for (var i = 0; i < a.Fields.Length; i++)
        {
            if (a.Fields[i].Name != b.Fields[i].Name) throw new UnifyException("records with different fields");
            Values(mc, width, a.Fields[i].Value, b.Fields[i].Value);
        }
    }

    /// <summary>The values a module, struct, record or signature holds, for the occurs check.</summary>
    private static IEnumerable<Value> Contents(MetaContext mc, Value value) => Nbe.Force(mc, value) switch
    {
        Value.VModule m => m.Entries.OfType<ModuleEntry.Field>().Select(f => f.Value),
        Value.VStruct st => st.Entries.OfType<ModuleEntry.Field>().Select(f => f.Value),
        Value.VRecord r => [r.Type, .. r.Fields.Select(f => f.Value)],
        Value.VSig sig => [Nbe.ApplyClosure(mc, sig.Body, new Value.VVar(0, []))],
        _ => [],
    };

    private static List<ModuleEntry.Field> Members(EquatableArray<ModuleEntry> entries, Func<MemberKind, bool> keep) =>
        [.. entries.OfType<ModuleEntry.Field>().Where(f => keep(f.Kind))];
}
