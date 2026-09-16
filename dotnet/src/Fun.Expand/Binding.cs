using Fun.Kernel;

namespace Fun.Expand;

/// <summary>
/// Whether a binding names a value, a procedural macro, or a syntactic role the
/// enforester reads. Name resolution returns values and macros; a
/// <see cref="Role"/> binder is what the enforester consults, and what a value
/// binder of its name conflicts with (M7).
/// </summary>
public enum BindingKind { Value, Macro, Role }

public sealed record BindingInfo(ScopeSet Scope, string ResolvedName, BindingKind Kind);

/// <summary>Raised when source cannot be read as syntax.</summary>
public sealed class ExpandException(string message) : Exception(message);

/// <summary>
/// The scope-aware binding table: every binder of a written name, with the
/// scope set it was written at.
/// </summary>
public sealed class BindingTable
{
    // Most recently added first, so binders with equal scope sets resolve to
    // the innermost.
    private readonly Dictionary<string, List<BindingInfo>> _bindings = [];

    public void Extend(string name, ScopeSet scope, string resolvedName, BindingKind kind = BindingKind.Value)
    {
        if (!_bindings.TryGetValue(name, out var existing)) _bindings[name] = existing = [];
        existing.Insert(0, new BindingInfo(scope, resolvedName, kind));
    }

    /// <summary>
    /// Resolves an occurrence: among the binders of its written name whose scope
    /// set is a subset of the occurrence's, the one with the largest set wins.
    /// Two candidates where neither set contains the other are ambiguous, and
    /// that is an error rather than a choice.
    /// </summary>
    public BindingInfo? Resolve(Id id)
    {
        if (!_bindings.TryGetValue(id.Name, out var candidates)) return null;

        BindingInfo? best = null;
        foreach (var info in candidates)
        {
            if (info.Kind == BindingKind.Role || !info.Scope.IsSubsetOf(id.Scope)) continue;
            if (best is null) { best = info; continue; }
            if (best.Scope.IsSubsetOf(info.Scope)) best = info;
            else if (!info.Scope.IsSubsetOf(best.Scope))
                throw new ExpandException(
                    $"ambiguous binding for {id.Name}: scopes {best.Scope} and {info.Scope}");
        }
        return best;
    }
}
