using Fun.Kernel;

namespace Fun.Expand;

public sealed partial class Expander
{
    /// <summary>
    /// A pattern synonym's parameters and its right-hand side's binders are one
    /// set of binders over the right-hand side, as a match arm's are over its
    /// pattern; a parameter and a binder of its name are the same binder.
    /// </summary>
    private Syntax ExpandPatternSynonym(Syntax.PatternSynonym synonym)
    {
        var scopes = synonym.Params.Concat(synonym.Rhs.Binders())
            .DistinctBy(id => id.Name)
            .Aggregate(ScopeSet.Empty, (acc, binder) => acc.Union(Bind(binder).Scope));
        var parameters = synonym.Params.Select(p =>
        {
            var marked = p with { Scope = p.Scope.Union(scopes) };
            return _bindings.Resolve(marked) is { } binder
                ? marked with { Name = binder.ResolvedName }
                : throw new InvalidOperationException($"a synonym parameter `{p.Name}` resolves to no binder");
        });
        return synonym with { Params = [.. parameters], Rhs = ExpandPattern(synonym.Rhs.AddScope(scopes)) };
    }
}
