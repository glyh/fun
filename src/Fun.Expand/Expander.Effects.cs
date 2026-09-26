using Fun.Kernel;

namespace Fun.Expand;

public sealed partial class Expander
{
    /// <summary>
    /// The syntax effects add. The effect's name binds over the rest of the block;
    /// its parameters bind over its operations, each seeing the ones before it.
    /// </summary>
    private Syntax? ExpandEffects(Syntax stx)
    {
        switch (stx)
        {
            case Syntax.EffectDef d:
            {
                var (scope, resolved) = Bind(d.Name);
                var (parameters, ops) = ExpandEffectFamily(d.Params, d.Ops);
                return d with
                {
                    Name = Rename(d.Name, scope, resolved),
                    Params = parameters,
                    Ops = ops,
                    Body = Expand(d.Body.AddScope(scope)),
                };
            }

            case Syntax.Perform p:
                return p with { Operation = (Syntax.FieldAccess)Expand(p.Operation), Arg = Expand(p.Arg) };

            case Syntax.Resume r:
                return r with { Arg = Expand(r.Arg) };

            default:
                return null;
        }
    }

    /// <summary>An effect family's parameters and operations, the operations inside every parameter.</summary>
    private (EquatableArray<Id>, EquatableArray<EffectOp>) ExpandEffectFamily(EquatableArray<Id> written, EquatableArray<EffectOp> ops)
    {
        var parameters = new List<Id>();
        var scopes = ScopeSet.Empty;
        foreach (var param in written)
        {
            var (scope, resolved) = Bind(param with { Scope = param.Scope.Union(scopes) });
            parameters.Add(Rename(param, scope, resolved));
            scopes = scopes.Union(scope);
        }
        return ([.. parameters],
                [.. ops.Select(o => o with { Input = Expand(o.Input.AddScope(scopes)), Output = Expand(o.Output.AddScope(scopes)) })]);
    }

    private EffectRow? ExpandRow(EffectRow? row, ScopeSet scope) =>
        row is null ? null : row with
        {
            Effects = [.. row.Effects.Select(e => Expand(e.AddScope(scope)))],
            Tails = [.. row.Tails.Select(t => Expand(t.AddScope(scope)))],
        };

    /// <summary>An effect family as a module item: its name binds over the items after it.</summary>
    private (ScopeSet Scope, Binding Expanded) ExpandEffectBinding(Binding.Effect e)
    {
        var (scope, resolved) = Bind(e.Name);
        var (parameters, ops) = ExpandEffectFamily(e.Params, e.Ops);
        return (scope, e with { Name = Rename(e.Name, scope, resolved), Params = parameters, Ops = ops });
    }
}
