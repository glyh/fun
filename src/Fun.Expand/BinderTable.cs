using Fun.Kernel;

namespace Fun.Expand;

/// <summary>
/// Whether a binding names a value, a procedural macro, or a syntactic role the
/// enforester reads. Name resolution returns values and macros; a
/// <see cref="Role"/> binder is what the enforester consults, and what a value
/// binder of its name conflicts with (M7).
/// </summary>
public enum BinderMeaning { Value, Macro, Role }

/// <summary>A binder: its scope set, its resolved name, what it means, and a role binder's role.</summary>
public sealed record Binder(ScopeSet Scope, string ResolvedName, BinderMeaning Kind, Role? Role = null, EquatableArray<HoleKind>? MacroParams = null)
{
    /// <summary>An order group's name is a role binder of its own sort: it never mixes with another binder of its name.</summary>
    public bool IsGroup => Role?.Meaning is RoleMeaning.OrderGroup;
}

/// <summary>Raised when source cannot be read as syntax.</summary>
public sealed class ExpandException(string message) : Exception(message);

/// <summary>
/// A role error that is genuine whatever the prelude binds: every binder and
/// group it names is already resolved (M7, order groups). Unlike an
/// <see cref="ExpandException"/>, it never stems from a role not yet ported.
/// </summary>
public sealed class RoleException(string message) : Exception(message);

/// <summary>
/// The binder table: every binder of a written name, with the
/// scope set it was written at.
/// </summary>
public sealed class BinderTable
{
    // Most recently added first, so binders with equal scope sets resolve to
    // the innermost.
    private readonly Dictionary<string, List<Binder>> _bindings = [];

    public void Extend(string name, ScopeSet scope, string resolvedName, BinderMeaning kind = BinderMeaning.Value, Role? role = null, EquatableArray<HoleKind>? macroParams = null)
    {
        if (!_bindings.TryGetValue(name, out var existing)) _bindings[name] = existing = [];
        existing.Insert(0, new Binder(scope, resolvedName, kind, role, macroParams));
    }

    /// <summary>Every binder, with its written name.</summary>
    public IEnumerable<(string Name, Binder Binder)> All() =>
        _bindings.SelectMany(entry => entry.Value.Select(binder => (entry.Key, binder)));

    /// <summary>Every binder of a written name, most recent first.</summary>
    public IReadOnlyList<Binder> Candidates(string name) =>
        _bindings.TryGetValue(name, out var found) ? found : [];

    /// <summary>
    /// A copy whose later binders do not reach this table: the roles quoted
    /// syntax declares while it is read stay inside it.
    /// </summary>
    public BinderTable Copy()
    {
        var copy = new BinderTable();
        foreach (var (name, binders) in _bindings) copy._bindings[name] = [.. binders];
        return copy;
    }

    /// <summary>
    /// Resolves an occurrence: among the binders of its written name whose scope
    /// set is a subset of the occurrence's, the one with the largest set wins.
    /// Two candidates where neither set contains the other are ambiguous, and
    /// that is an error rather than a choice.
    /// </summary>
    public Binder? Resolve(Id id) =>
        Best(id.Name, Candidates(id.Name).Where(b => b.Kind != BinderMeaning.Role && b.Scope.IsSubsetOf(id.Scope)));

    /// <summary>A syntactic role is resolved like any binder (M7), among the roles of that name and fixity.</summary>
    public Role? FindRole(string name, Fixity fixity, ScopeSet scope) =>
        Best(name, Candidates(name).Where(b =>
            b.Role is { } role && !b.IsGroup && role.Fixity == fixity && b.Scope.IsSubsetOf(scope)))?.Role;

    /// <summary>An order group, resolved by scope set like any binder.</summary>
    public Order? FindOrder(string name, ScopeSet scope) =>
        Best(name, Candidates(name).Where(b => b.IsGroup && b.Scope.IsSubsetOf(scope)))?.Role?.Order;

    private static Binder? Best(string name, IEnumerable<Binder> candidates)
    {
        Binder? best = null;
        foreach (var info in candidates)
        {
            if (best is null) { best = info; continue; }
            // Equal sets keep the more recent binder, which came first.
            if (info.Scope.IsSubsetOf(best.Scope)) continue;
            if (best.Scope.IsSubsetOf(info.Scope)) best = info;
            else if (!info.Scope.IsSubsetOf(best.Scope))
                throw new ExpandException($"ambiguous binding for {name}: scopes {best.Scope} and {info.Scope}");
        }
        return best;
    }
}
