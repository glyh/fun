using Fun.Kernel;

namespace Fun.Expand;

public sealed partial class Expander
{
    /// <summary>
    /// A method binds its name for the bindings after it, and its parameters in
    /// order: each parameter's type sees the parameters before it, the body sees
    /// them all.
    /// </summary>
    private (ScopeSet Scope, Binding.Method Method) ExpandMethod(Binding.Method m)
    {
        var (scope, resolved) = Bind(m.Name);
        var paramScopes = ScopeSet.Empty;
        var parameters = new List<Param>();
        foreach (var raw in m.Params)
        {
            var param = paramScopes.IsEmpty ? raw : raw with
            {
                Name = raw.Name with { Scope = raw.Name.Scope.Union(paramScopes) },
                Type = raw.Type?.AddScope(paramScopes),
            };
            var (paramScope, paramResolved) = Bind(param.Name);
            parameters.Add(param with
            {
                Name = Rename(param.Name, paramScope, paramResolved),
                Type = param.Type is null ? null : Expand(param.Type),
            });
            paramScopes = paramScopes.Union(paramScope);
        }
        return (scope, m with
        {
            Name = Rename(m.Name, scope, resolved),
            Params = [.. parameters],
            Body = Expand(m.Body.AddScope(paramScopes)),
            // The row sits on the innermost arrow, so it reads the parameters: `->{Mutate(r)}`.
            Row = ExpandRow(m.Row, paramScopes),
        });
    }
}
