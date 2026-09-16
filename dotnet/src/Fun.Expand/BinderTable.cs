using Fun.Kernel;

namespace Fun.Expand;

/// <summary>
/// Whether a binding names a value, a procedural macro, or a syntactic role the
/// enforester reads. Name resolution returns values and macros; a
/// <see cref="Role"/> binder is what the enforester consults, and what a value
/// binder of its name conflicts with (M7).
/// </summary>
public enum BinderMeaning { Value, Macro, Role }

public sealed record Binder(ScopeSet Scope, string ResolvedName, BinderMeaning Kind);

/// <summary>Raised when source cannot be read as syntax.</summary>
public sealed class ExpandException(string message) : Exception(message);

/// <summary>
/// The binder table: every binder of a written name, with the
/// scope set it was written at.
/// </summary>
public sealed class BinderTable
{
    // Most recently added first, so binders with equal scope sets resolve to
    // the innermost.
    private readonly Dictionary<string, List<Binder>> _bindings = [];

    public void Extend(string name, ScopeSet scope, string resolvedName, BinderMeaning kind = BinderMeaning.Value)
    {
        if (!_bindings.TryGetValue(name, out var existing)) _bindings[name] = existing = [];
        existing.Insert(0, new Binder(scope, resolvedName, kind));
    }

    /// <summary>
    /// Resolves an occurrence: among the binders of its written name whose scope
    /// set is a subset of the occurrence's, the one with the largest set wins.
    /// Two candidates where neither set contains the other are ambiguous, and
    /// that is an error rather than a choice.
    /// </summary>
    public Binder? Resolve(Id id)
    {
        if (!_bindings.TryGetValue(id.Name, out var candidates)) return null;

        Binder? best = null;
        foreach (var info in candidates)
        {
            if (info.Kind == BinderMeaning.Role || !info.Scope.IsSubsetOf(id.Scope)) continue;
            if (best is null) { best = info; continue; }
            if (best.Scope.IsSubsetOf(info.Scope)) best = info;
            else if (!info.Scope.IsSubsetOf(best.Scope))
                throw new ExpandException(
                    $"ambiguous binding for {id.Name}: scopes {best.Scope} and {info.Scope}");
        }
        return best;
    }
}
