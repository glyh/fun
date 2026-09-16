using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Elaborator
{
    /// <summary>
    /// An export: each member leaves as a public entry of this module, pushed
    /// through the slot list (I2) under a key nothing spells, so it opens nothing
    /// here. Every member's term was built before any of them was pushed, so the
    /// ith is read i entries further in.
    /// </summary>
    private static Context InferExport(Context ctx, Binding.Export export, List<BindingTerm> terms, List<ModuleEntry> entries)
    {
        if (!export.Public) return ctx;

        var members = ExportedMembers(ctx, export.Of);
        if (export.Names is { } names)
            members = [.. names.Select(n => members.Any(m => m.Name == n)
                ? members.Last(m => m.Name == n)
                : throw new FunException($"export of unknown member `{n}`"))];

        for (var i = 0; i < members.Count; i++)
        {
            var (name, term, type, constructor) = members[i];
            var binding = new BindingTerm.Let(name, MemberKind.Public, term.Shift(i));
            ctx = ExtendFromSlots(ctx, binding, [($"{name}#export", type)]);
            terms.Add(binding);
            entries.Add(new ModuleEntry.Field(name, MemberKind.Public, type) { Constructor = constructor });
        }
        return ctx;
    }

    /// <summary>What <c>export M</c> takes: an enum's constructors, or a module's public members, in order.</summary>
    private static List<(string Name, Term Term, Value Type, ConstructorMark? Constructor)> ExportedMembers(Context ctx, Syntax of)
    {
        var (term, type) = Infer(ctx, of);

        if (PeelFormer(ctx, ctx.Eval(term), type) is var (nominal, _))
            return [.. nominal.Decl.Constructors.Select(c =>
            {
                var (member, memberType) = ConstructorMember(ctx, term, type, c.Name)!.Value;
                return (c.Name, member, memberType, (ConstructorMark?)new ConstructorMark(ctx.Eval(term), ctx.Force(type), c));
            })];

        return ctx.Force(ModuleTypeOf(ctx, type, term)) switch
        {
            Value.VModule module => [.. module.Entries.OfType<ModuleEntry.Field>()
                .Where(f => f.Kind == MemberKind.Public)
                .Select(f => (f.Name, (Term)new Term.Dot(term, f.Name), ctx.Force(f.Value), f.Constructor))],
            Value.VMeta or Value.VVar or Value.VNeutral =>
                throw new NotImplementedException("not ported yet: exporting a value of unknown type"),
            _ => throw new FunException("export of a non-module"),
        };
    }

    /// <summary>
    /// A module's member names may not clash through an export: an exported name
    /// is taken by no other public member, before or after it, and no export takes
    /// a name already public - except a constructor exported from the enum it
    /// shares its name with, which the path then denotes (I3).
    /// </summary>
    private sealed class ExportClashes
    {
        private readonly HashSet<string> _exported = [];
        private readonly HashSet<string> _seen = [];

        public void Check(Binding binding, IEnumerable<ModuleEntry> added)
        {
            var export = binding as Binding.Export;
            // The exemption holds only when the export names this module's own enum
            // binder: an enum reached through an open is not a member here, so a
            // public member of its name is a different binding and clashes.
            var source = export?.Of is Syntax.Var v ? Label(v.Id.Name) : null;
            foreach (var name in added.OfType<ModuleEntry.Field>().Where(f => f.Kind == MemberKind.Public).Select(f => f.Name))
            {
                if (_exported.Contains(name) || export is not null && _seen.Contains(name) && name != source)
                    throw new FunException($"export clash: `{name}` is already a member");
                if (export is not null) _exported.Add(name);
                _seen.Add(name);
            }
        }
    }
}
