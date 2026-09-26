using Fun.Kernel;

namespace Fun.Expand;

public sealed partial class Expander
{
    /// <summary>
    /// Each arm binds its pattern's names once apiece - an or-pattern's
    /// alternatives share them - and both the pattern and the result sit inside
    /// every one of those binders.
    /// </summary>
    private Syntax ExpandMatch(Syntax.Match match) => match with
    {
        Scrutinee = Expand(match.Scrutinee),
        Branches = [.. match.Branches.Select(branch =>
        {
            // An effect branch's operation is outside the binders its argument pattern writes.
            var operation = branch.Operation is null ? null : (Syntax.FieldAccess)Expand(branch.Operation);
            var scopes = branch.Pattern.Binders()
                .Aggregate(ScopeSet.Empty, (acc, binder) => acc.Union(Bind(binder).Scope));
            return new MatchBranch(ExpandPattern(branch.Pattern.AddScope(scopes)), Expand(branch.Body.AddScope(scopes))) { Operation = operation };
        })],
    };

    private Pattern ExpandPattern(Pattern pattern) => pattern switch
    {
        Pattern.Wild or Pattern.Atom => pattern,
        Pattern.Bind b => _bindings.Resolve(b.Name) is { } binder
            ? b with { Name = b.Name with { Name = binder.ResolvedName } }
            : throw new InvalidOperationException($"a pattern binder `{b.Name.Name}` resolves to no binder"),
        Pattern.Prod p => p with { Items = [.. p.Items.Select(ExpandPattern)] },
        Pattern.Or o => o with { Left = ExpandPattern(o.Left), Right = ExpandPattern(o.Right) },
        // A constructor's head is an occurrence like any other.
        Pattern.Con c => c with { Head = Expand(c.Head), Args = [.. c.Args.Select(ExpandPattern)] },
        Pattern.Record r => r with { Type = Expand(r.Type), Fields = [.. r.Fields.Select(f => (f.Name, ExpandPattern(f.Pattern)))] },
        Pattern.AtomType => pattern,
        Pattern.StructType s => s with { Fields = [.. s.Fields.Select(f => (f.Name, ExpandPattern(f.Pattern)))] },
        Pattern.SynonymParam => pattern,
        _ => throw new InvalidOperationException($"unhandled pattern {pattern.GetType().Name}"),
    };
}
