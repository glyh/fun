using Quill.Kernel;

namespace Quill.Compiler;

public static partial class Elaborator
{
    /// <summary>
    /// A selective open <c>open M.{a, b}</c>: the list is the width (decided
    /// 2026-09-28) — everything <c>export</c>'s selection carries, only what it
    /// names arriving. A null list is the wholesale <c>open M</c>.
    /// </summary>
    private static bool Selects(EquatableArray<string>? names, string? name) =>
        names is null || name is not null && names.Contains(name);

    /// <summary>
    /// The names a selection may carry from an imported unit beyond its value
    /// members: its roles and macros, which the expander binds in the open's
    /// region (decision 1). They are not entries here, so naming one would
    /// otherwise read as unknown.
    /// </summary>
    private static HashSet<string> UnitSurfaceNames(Context ctx, Syntax? of)
    {
        var names = new HashSet<string>();
        if (of is not Syntax.Import import || ctx.Loader is not { } loader) return names;
        var unit = loader.LoadSyntax(import.Path);
        foreach (var (name, _) in unit.Roles) names.Add(name);
        foreach (var (name, _) in unit.Macros) names.Add(name);
        return names;
    }

    /// <summary>
    /// The names a module's entries are members under: a public field, or a named
    /// public impl. A selection naming anything else is unknown.
    /// </summary>
    private static IEnumerable<string> PublicMemberNames(EquatableArray<ModuleEntry> entries)
    {
        foreach (var entry in entries)
        {
            var name = entry switch
            {
                ModuleEntry.Field { Kind: MemberKind.Public } f => f.Name,
                ModuleEntry.Impl { Kind: MemberKind.Public, Name: { } n } => n,
                _ => null,
            };
            if (name is not null) yield return name;
        }
    }

    /// <summary>
    /// An unknown name is an error in the export form's shape: <c>export M.{nope}</c>
    /// reports "export of unknown member", so the open side answers the same way
    /// rather than inventing a second behaviour.
    /// </summary>
    private static void CheckOpenNames(Context ctx, Syntax? of, EquatableArray<string>? names, IEnumerable<string> known)
    {
        if (names is null) return;
        var knownNames = new HashSet<string>(known);
        knownNames.UnionWith(UnitSurfaceNames(ctx, of));
        foreach (var name in names)
            if (!knownNames.Contains(name))
                throw new FunException($"open of unknown member `{name}`");
    }
}
