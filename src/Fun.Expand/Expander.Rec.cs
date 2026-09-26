using Fun.Kernel;

namespace Fun.Expand;

public sealed partial class Expander
{
    /// <summary>
    /// A <c>rec … and …</c> group's members: every member is bound first, so every
    /// value sees every member. Returns the members and the scopes they introduced.
    /// </summary>
    private (EquatableArray<RecMember> Members, ScopeSet Scopes) ExpandRecMembers(EquatableArray<RecMember> members)
    {
        var bound = members.Select(m => (Member: m, Binder: Bind(m.Name))).ToList();
        var scopes = bound.Aggregate(ScopeSet.Empty, (acc, b) => acc.Union(b.Binder.Scope));
        return ([.. bound.Select(b => new RecMember(
                    Rename(b.Member.Name, b.Binder.Scope, b.Binder.Resolved),
                    Expand(b.Member.Value.AddScope(scopes))))],
                scopes);
    }

    private Syntax ExpandLetRecGroup(Syntax.LetRecGroup group)
    {
        var (members, scopes) = ExpandRecMembers(group.Members);
        return group with { Members = members, Body = Expand(group.Body.AddScope(scopes)) };
    }

    /// <summary>A module's recursive group, taking the scopes of the bindings before it; returns the scopes it adds.</summary>
    private ScopeSet ExpandRecGroupBinding(Binding.RecGroup group, ScopeSet active, List<Binding> expanded)
    {
        var (members, scopes) = ExpandRecMembers(((Binding.RecGroup)group.AddScope(active)).Members);
        expanded.Add(group with { Members = members });
        return active.Union(scopes);
    }
}
