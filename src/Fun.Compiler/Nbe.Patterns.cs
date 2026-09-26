using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Nbe
{
    /// <summary>What matching one pattern produced: matched, no match, or stuck on an unknown value.</summary>
    private abstract record MatchResult
    {
        public sealed record Matched : MatchResult;

        public sealed record NoMatch : MatchResult;

        public sealed record Stuck(Value Value) : MatchResult;
    }

    /// <summary>
    /// The first arm whose pattern matches, with its binders pushed in the order
    /// the arm writes them.
    /// </summary>
    private static MatchStep SelectArmInOrder(MetaContext mc, Environment env, Value scrutinee, Term.Match match, DecisionTree.Sequential arms)
    {
        for (var i = 0; i < arms.Arms.Length; i++)
        {
            var binds = new List<(Occurrence At, Value Value)>();
            switch (Matches(mc, env, arms.Arms[i], scrutinee, Occurrence.Base.Instance, binds))
            {
                case MatchResult.Stuck stuck:
                    return new MatchStep.Stuck(stuck.Value);
                case MatchResult.NoMatch:
                    continue;
                case MatchResult.Matched:
                    var arm = MatchCompile.InSourceOrder(binds, b => b.At).Aggregate(env, (e, b) => e.Push(b.Value));
                    return new MatchStep.Arm(arm, match.Bodies[i]);
            }
        }
        throw new InvalidOperationException("no arm matched: the match was checked exhaustive");
    }

    /// <summary>Whether <paramref name="value"/> matches <paramref name="pattern"/>, collecting its binders by position.</summary>
    private static MatchResult Matches(MetaContext mc, Environment env, CorePattern pattern, Value value, Occurrence at, List<(Occurrence, Value)> binds)
    {
        switch (pattern)
        {
            case CorePattern.Wild:
                return new MatchResult.Matched();
            case CorePattern.Bind:
                binds.Add((at, value));
                return new MatchResult.Matched();
            case CorePattern.Or or:
            {
                var mark = binds.Count;
                if (Matches(mc, env, or.Left, value, at, binds) is MatchResult.Matched) return new MatchResult.Matched();
                binds.RemoveRange(mark, binds.Count - mark);
                return Matches(mc, env, or.Right, value, at, binds);
            }
        }

        switch (pattern, Force(mc, value))
        {
            case (CorePattern.Atom a, Value.VAtom v):
                return a.Value == v.Atom ? new MatchResult.Matched() : new MatchResult.NoMatch();
            case (CorePattern.AtomType t, Value.VAtomTy v):
                return t.Ty == v.Ty ? new MatchResult.Matched() : new MatchResult.NoMatch();
            case (CorePattern.Prod p, Value.VProd v) when p.Items.Length == v.Items.Length:
                return AllMatch(p.Items.Select((item, i) => Matches(mc, env, item, v.Items[i], new Occurrence.Child(at, i), binds)));
            case (CorePattern.Con c, Value.VCon v):
                return MatchCon(mc, env, c, v, at, binds);
            case (CorePattern.Record r, Value.VRecord v):
                return AllMatch(r.Fields.Select(f => Matches(mc, env, f.Pattern, v.Fields.Last(field => field.Name == f.Name).Value, new Occurrence.Field(at, f.Name), binds)));
            case (CorePattern.StructType s, Value.VStruct v):
            {
                var fields = v.Entries.OfType<ModuleEntry.Field>().Where(f => f.Kind == MemberKind.Field).ToList();
                if (!s.Partial && fields.Count != s.Fields.Length) return new MatchResult.NoMatch();
                return AllMatch(s.Fields.Select(f => fields.LastOrDefault(field => field.Name == f.Name) is { } field
                    ? Matches(mc, env, f.Pattern, field.Value, new Occurrence.Field(at, f.Name), binds)
                    : new MatchResult.NoMatch()));
            }
            case (CorePattern.NominalHead h, Value.VNominal v):
                return MatchesNominalHead(mc, env, h, v, at, binds);
            case (_, (Value.VNeutral or Value.VVar or Value.VMeta) and var stuck):
                return new MatchResult.Stuck(stuck);
            default:
                return new MatchResult.NoMatch();
        }
    }

    /// <summary>Every sub-pattern matched; the first that did not is returned (a no-match, or a stuck value).</summary>
    private static MatchResult AllMatch(IEnumerable<MatchResult> results)
    {
        foreach (var result in results)
        {
            if (result is not MatchResult.Matched) return result;
        }
        return new MatchResult.Matched();
    }

    /// <summary>A constructor pattern: the tag must agree, then every payload pattern.</summary>
    private static MatchResult MatchCon(MetaContext mc, Environment env, CorePattern.Con c, Value.VCon v, Occurrence at, List<(Occurrence, Value)> binds)
    {
        if (c.Name != v.Name) return new MatchResult.NoMatch();
        return AllMatch(c.Args.Select((arg, i) => Matches(mc, env, arg, v.Args[i], new Occurrence.Payload(at, c.Name, i), binds)));
    }
}
