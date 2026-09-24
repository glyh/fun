using Fun.Kernel;

namespace Fun.Expand;

public sealed partial class Expander
{
    private readonly IMacroRuntime _runtime;

    /// <summary>The unit a binder like <c>M = import "u"</c> is a handle on, by the binder's resolved name.</summary>
    private readonly Dictionary<string, string> _moduleUnits = [];

    /// <summary>The unit each intro scope's application came from, when the form applied was imported.</summary>
    private readonly Dictionary<int, string> _introScopeUnits = [];

    /// <summary>Per open label, the names of the roles visible in that open's region (M7).</summary>
    private readonly Dictionary<string, SortedSet<string>> _openRoles = [];

    private readonly List<(string Name, Role Role)> _syntaxExports = [];

    /// <summary>This unit's public members that are unit handles, e.g. <c>pub M = import "m"</c>.</summary>
    private readonly List<(string Name, string Path)> _ownUnitMembers = [];

    public Expander(IMacroRuntime runtime)
    {
        _runtime = runtime;
        AddBaseRoles(_bindings);
    }

    /// <summary>The public roles this expansion declared or re-exported: a unit's syntax exports.</summary>
    public UnitSyntax SyntaxExports => new([.. _syntaxExports])
    {
        Macros = [.. _macroExports],
        UnitMembers = [.. _ownUnitMembers],
    };

    /// <summary>The label of an open of <c>import "path"</c>: every such open names the same unit.</summary>
    public static string UnitOpenLabel(string path) => $"unit:{path}";

    /// <summary>Where an open's module expression is written: the scope set the roles it brings bind from.</summary>
    private static ScopeSet Occurrence(Syntax of) => of switch
    {
        Syntax.Var v => v.Id.Scope,
        Syntax.Import i => i.Scope,
        _ => ScopeSet.Empty,
    };

    /// <summary>
    /// The unit an expression denotes: an import, a binder bound to one, or a dotted
    /// path whose members name unit-valued members of the unit before them.
    /// </summary>
    private string? UnitPathOf(Syntax of) => of switch
    {
        Syntax.Import i => i.Path,
        Syntax.Var v => (v.Id.Name.Contains('#') ? v.Id.Name : _bindings.Resolve(v.Id)?.ResolvedName) is { } resolved
            ? _moduleUnits.GetValueOrDefault(resolved)
            : null,
        Syntax.FieldAccess { Of: var inner, Field: var field } => UnitPathOf(inner) is { } path
            ? _runtime.LoadSyntax(path).UnitMembers.FirstOrDefault(m => m.Name == field).Path
            : null,
        _ => null,
    };

    /// <summary>The roles the unit <paramref name="of"/> denotes exports; null when it denotes no unit.</summary>
    private EquatableArray<(string Name, Role Role)>? UnitRoles(Syntax of) =>
        UnitPathOf(of) is { } path ? _runtime.LoadSyntax(path).Roles : null;

    /// <summary>
    /// Enters an open of <paramref name="of"/>: a fresh scope marks its region, and a
    /// label names it -- the unit's own for an import, or for a handle bound to one
    /// (<c>Core = import "std"; open Core</c>), so the ids a unit's syntax form
    /// introduces find that unit's members through it. The roles visible where it is
    /// written are noted against it, except an imported unit's own, opened with it.
    /// </summary>
    private (ScopeSet Scope, string Label) EnterOpen(Syntax of)
    {
        var scope = _scopeCounter++;
        var label = UnitPathOf(of) is { } path ? UnitOpenLabel(path) : $"open:{scope}";
        var occurrence = Occurrence(of);
        foreach (var binder in _bindings.All())
            if (binder.Binder.Kind != BinderMeaning.Value && !binder.Binder.IsGroup
                && binder.Binder.Scope.IsSubsetOf(occurrence) && binder.Binder.ResolvedName != label)
                NoteOpenRole(label, binder.Name);
        _opens.Add((scope, label));
        return (ScopeSet.Singleton(scope), label);
    }

    private void NoteOpenRole(string label, string name)
    {
        if (!_openRoles.TryGetValue(label, out var names)) _openRoles[label] = names = [];
        names.Add(name);
    }

    private EquatableArray<string> RolesInRegion(string label) =>
        _openRoles.TryGetValue(label, out var names) ? [.. names] : [];

    /// <summary>
    /// A role binder declared inside an open's region is noted against that open,
    /// so the open may not supply its name -- unless the role is the opened unit's own.
    /// </summary>
    private void NoteRoleInOpens(string name, ScopeSet written, Role role)
    {
        foreach (var (scope, label) in _opens)
            if (written.Contains(scope) && !(role.FromUnit is { } unit && label == UnitOpenLabel(unit)))
                NoteOpenRole(label, name);
    }

    /// <summary>
    /// An import brings its unit's public roles into the region of the open or binder
    /// that imported it (M7). The unit's scopes mean nothing here, so they are dropped;
    /// the ids a role's replacement introduces mean the unit's names instead.
    /// </summary>
    private void ImportRoles(Syntax of, ScopeSet written, ScopeSet region, bool opened = false)
    {
        if (of is not Syntax.Import import) return;
        var roles = _runtime.LoadSyntax(import.Path).Roles;
        CheckDuplicateExports(roles);
        var unscoped = SyntaxMapper.OfIds(id => id with { Scope = ScopeSet.Empty });
        foreach (var (name, role) in roles)
            BindRoleAt(name, written, region, UnitOpenLabel(import.Path), unscoped.MapRole(role) with { FromUnit = import.Path });
        // A unit's macros are members of it: an open binds them bare in its region, a handle reaches them as M.m.
        if (opened)
            foreach (var (name, macro) in _runtime.LoadSyntax(import.Path).Macros)
                _bindings.Extend(name, written.Union(region), UnitMacroKey(import.Path, name), BinderMeaning.Macro, macroParams: macro.Params);
    }

    /// <summary>
    /// The key a unit's macro is known by here: a resolved name no source can spell,
    /// the same for every import of the unit.
    /// </summary>
    private static string UnitMacroKey(string path, string name) => $"{name}#unit:{path}";

    /// <summary>A unit's public macro, by its key, registered where it is first reached.</summary>
    private MacroEntry? UnitMacro(string path, string name)
    {
        var key = UnitMacroKey(path, name);
        if (_macros.TryGetValue(key, out var known)) return known;
        var exported = _runtime.LoadSyntax(path).Macros.FirstOrDefault(m => m.Name == name).Macro;
        if (exported is not null) _macros[key] = exported;
        return exported;
    }

    /// <summary>A unit exporting two roles of one name, fixity and sort is ambiguous wherever it is imported.</summary>
    private static void CheckDuplicateExports(EquatableArray<(string Name, Role Role)> roles)
    {
        var seen = new Dictionary<(string, Fixity, bool), Role>();
        foreach (var (name, role) in roles)
        {
            var key = (name, role.Fixity, role.Meaning is RoleMeaning.OrderGroup);
            if (seen.TryGetValue(key, out var previous))
                throw new ExpandException(
                    $"ambiguous syntax extension candidates for {role.Fixity.ToString().ToLowerInvariant()} operator \"{name}\": " +
                    $"declarations at {previous.DeclaredAt} and {role.DeclaredAt}");
            seen[key] = role;
        }
    }

    /// <summary>
    /// <c>M = import "u"</c>: <c>M</c> is a handle on the unit, and the unit's roles bind in the binder's
    /// region. A public binding also names the unit among this unit's own members, so a path
    /// through it (<c>W.M.g</c> in an importer) resolves at any depth.
    /// </summary>
    private void BindImportHandle(Syntax value, Id written, ScopeSet region, string resolved, bool publicBinding = false)
    {
        if (value is Syntax.Import import)
        {
            _moduleUnits[resolved] = import.Path;
            if (publicBinding) _ownUnitMembers.Add((resolved.IndexOf('#') is var at and >= 0 ? resolved[..at] : resolved, import.Path));
        }
        ImportRoles(value, written.Scope, region);
    }

    /// <summary><c>export M</c> of a unit re-exports its public roles with its values (the selection applies to both).</summary>
    private void ExportUnitRoles(Binding.Export export)
    {
        if (!export.Public || UnitPathOf(export.Of) is not { } path) return;
        foreach (var (name, role) in _runtime.LoadSyntax(path).Roles)
            if (export.Names is not { } names || names.Contains(name))
                _syntaxExports.Add((name, role with { FromUnit = path }));
    }
}
