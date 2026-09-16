using Fun.Kernel;

namespace Fun.Expand;

public sealed partial class Expander
{
    /// <summary>
    /// A trait binds its name over the rest; its parameter binds over its operation
    /// types. An impl binds nothing: its trait path's head is an occurrence, its
    /// operation names are labels, and an impl name is a member label.
    /// </summary>
    private Syntax ExpandTraits(Syntax stx)
    {
        switch (stx)
        {
            case Syntax.TraitDef t:
            {
                var (scope, resolved) = Bind(t.Name);
                var (param, traitFields) = ExpandTraitFields(t.Param, t.Fields);
                return t with { Name = Rename(t.Name, scope, resolved), Param = param, Fields = traitFields, Body = Expand(t.Body.AddScope(scope)) };
            }

            case Syntax.ImplDef i:
                return i with
                {
                    TraitPath = Expand(i.TraitPath),
                    Arg = Expand(i.Arg),
                    Fields = [.. i.Fields.Select(f => (f.Name, Expand(f.Value)))],
                    Body = Expand(i.Body),
                };

            case Syntax.TraitBoundSet b:
                return b with { Traits = [.. b.Traits.Select(Expand)] };

            default:
                throw new InvalidOperationException($"unhandled syntax {stx.GetType().Name}");
        }
    }

    /// <summary>A trait or impl as a module or struct item; the scopes it adds for the items after it.</summary>
    private ScopeSet ExpandTraitBinding(Binding binding, ScopeSet active, List<Binding> expanded)
    {
        switch (binding.AddScope(active))
        {
            case Binding.Trait t:
            {
                var (scope, resolved) = Bind(t.Name);
                var (param, traitFields) = ExpandTraitFields(t.Param, t.Fields);
                expanded.Add(t with { Name = Rename(t.Name, scope, resolved), Param = param, Fields = traitFields });
                return active.Union(scope);
            }

            case Binding.Impl i:
                expanded.Add(i with
                {
                    TraitPath = Expand(i.TraitPath),
                    Arg = Expand(i.Arg),
                    Fields = i.Fields is { } fields ? [.. fields.Select(f => (f.Name, Expand(f.Value)))] : null,
                });
                return active;

            case var other:
                throw new InvalidOperationException($"unhandled binding {other.GetType().Name}");
        }
    }

    private (Id Param, EquatableArray<(string Name, Syntax Type)> Fields) ExpandTraitFields(
        Id param, EquatableArray<(string Name, Syntax Type)> fields)
    {
        var (scope, resolved) = Bind(param);
        return (Rename(param, scope, resolved), [.. fields.Select(f => (f.Name, Expand(f.Type.AddScope(scope))))]);
    }
}
