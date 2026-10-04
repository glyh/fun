using Quill.Kernel;

namespace Quill.Compiler;

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
            case (CorePattern.TupleType t, Value.VProdTy v) when t.Items.Length == v.Items.Length:
                return AllMatch(t.Items.Select((item, i) => Matches(mc, env, item, v.Items[i], new Occurrence.Child(at, i), binds)));
            case (CorePattern.Prod p, Value.VProdTy v) when p.Items.Length == v.Items.Length:
                return AllMatch(p.Items.Select((item, i) => Matches(mc, env, item, v.Items[i], new Occurrence.Child(at, i), binds)));
            case (CorePattern.Universe, Value.VU):
                return new MatchResult.Matched();
            // An explicit arrow matches an explicit Pi only, and only a codomain that
            // does not depend on its domain: `a -> b` is a plain function type. A
            // polymorphic type is an implicit Pi and does not match here.
            case (CorePattern.Arrow a, Value.VPi { Explicitness: Explicitness.Explicit } pi):
            {
                var domain = Matches(mc, env, a.Domain, pi.Domain, new Occurrence.Child(at, 0), binds);
                if (domain is not MatchResult.Matched) return domain;
                // The codomain is read at a fresh rigid variable, not at the domain.
                // No solving, nothing to restore; dependence on it means the arm does
                // not match and a later arm is tried, with no error (the scrutinee is
                // a run-time value).
                var binder = new Value.VVar(FreshBinderLevel(env.Count, pi.Codomain), []);
                var codomain = ApplyClosure(mc, pi.Codomain, binder);
                if (MentionsLevel(mc, binder.Level, codomain)) return new MatchResult.NoMatch();
                return Matches(mc, env, a.Codomain, codomain, new Occurrence.Child(at, 1), binds);
            }

            // `[a] -> b` matches an implicit Pi: the Pi's binder is bound to a fresh
            // rigid variable, and the codomain is matched as a pattern with it in
            // scope, so a mention of the name is a Pin to the binder.
            case (CorePattern.ImplicitArrow ia, Value.VPi { Explicitness: Explicitness.Implicit } pi):
            {
                var binder = new Value.VVar(FreshBinderLevel(env.Count, pi.Codomain), []);
                var codomain = ApplyClosure(mc, pi.Codomain, binder);
                binds.Add((new Occurrence.Child(at, 0), binder));
                return Matches(mc, env.Push(binder), ia.Codomain, codomain, new Occurrence.Child(at, 1), binds);
            }
            case (CorePattern.Pin p, var scrutinee):
            {
                var rooted = p.Width is 0 ? p.Term : p.Term.Shift(env.Count - p.Width);
                var pinned = Eval(mc, env, rooted);
                // A pinned test against a not-yet-known scrutinee parks (FMatch) -
                // unless the scrutinee IS the pinned value, which matches: the
                // implicit-arrow binder's own variable is a rigid variable known to
                // the pattern, not an unknown to wait on.
                if (scrutinee is Value.VNeutral or Value.VVar or Value.VMeta)
                    return scrutinee.Equals(pinned) ? new MatchResult.Matched() : new MatchResult.Stuck(scrutinee);
                return Convertible(mc, env.Count, scrutinee, pinned)
                    ? new MatchResult.Matched() : new MatchResult.NoMatch();
            }
            case (_, (Value.VNeutral or Value.VVar or Value.VMeta) and var stuck):
                return new MatchResult.Stuck(stuck);
            default:
                return new MatchResult.NoMatch();
        }
    }

    /// <summary>
    /// The level of a fresh rigid variable standing for a Pi's binder: above every
    /// variable the environment or the codomain's closure can hold, so nothing else
    /// matches it.
    /// </summary>
    private static int FreshBinderLevel(int envCount, Closure codomain) => Math.Max(envCount, codomain.Environment.Count);

    /// <summary>Whether <paramref name="value"/> mentions the rigid variable at <paramref name="level"/> (the occurs check, over levels).</summary>
    private static bool MentionsLevel(MetaContext mc, int level, Value value)
    {
        bool Go(Value v) => MentionsLevel(mc, level, v);
        return Force(mc, value) switch
        {
            Value.VVar r => r.Level == level || r.Spine.Any(Go),
            Value.VMeta m => m.Spine.Any(Go),
            Value.VPi pi => Go(pi.Domain) || Go(ApplyClosure(mc, pi.Codomain, new Value.VVar(level + 1, [])))
                            || Go(EvalRowClosure(mc, pi.Row, new Value.VVar(level + 1, []))),
            Value.VProd p => p.Items.Any(Go),
            Value.VProdTy p => p.Items.Any(Go),
            Value.VNominal n => n.Captures.Any(Go),
            Value.VEffectRow row => row.Effects.Concat(row.Tails).Any(Go),
            Value.VEffect e => e.Params.Any(Go),
            Value.VRefTy r => Go(r.Heap) || Go(r.Element),
            Value.VRecursiveOccurrence o => o.Captures.Concat(o.Args).Any(Go),
            Value.VNeutral n => n.Frames.Any(frame => frame switch
            {
                Frame.FApp app => Go(app.Arg),
                Frame.FRefSet set => Go(set.Value),
                _ => false,
            }),
            Value.VModule m => m.Entries.Any(e => Go(EntryValue(e))),
            Value.VStruct st => st.Entries.Any(e => Go(EntryValue(e))),
            Value.VRecord r => Go(r.Type) || r.Fields.Any(f => Go(f.Value)),
            Value.VSig sig => Go(ApplyClosure(mc, sig.Body, new Value.VVar(level + 1, []))),
            Value.VTraitDict d => d.Args.Concat(d.Operations.Select(o => o.Type)).Any(Go),
            _ => false,
        };

        static Value EntryValue(ModuleEntry entry) => entry switch
        {
            ModuleEntry.Field f => f.Value,
            ModuleEntry.Impl i => i.DictType,
            _ => Value.VU.Instance,
        };
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
