using Fun.Kernel;

namespace Fun.Compiler
{
    public static partial class Nbe
    {
        /// <summary>
        /// A match on a value whose head is unknown - a variable, a meta, a stuck
        /// computation, under the checker - waits as that value's last frame. Null when
        /// the scrutinee is known, so the decision tree chooses the arm.
        /// </summary>
        // ponytail: only an unknown *scrutinee* waits; a known scrutinee with an unknown
        // part a pattern tests still raises "not ported yet" in SelectArm.
        private static Value? StuckMatch(MetaContext mc, Value scrutinee, Environment env, Term.Match match)
        {
            var frame = new Frame.FMatch(env, match);
            return Force(mc, scrutinee) switch
            {
                Value.VNeutral n => n with { Ty = Value.VU.Instance, Frames = n.Frames.Add(frame) },
                Value.VVar v => new Value.VNeutral(Value.VU.Instance, new Head.HVar(v.Level), Spine(v.Spine).Add(frame)),
                Value.VMeta m => new Value.VNeutral(Value.VU.Instance, new Head.HMeta(m.Id), Spine(m.Spine).Add(frame)),
                _ => null,
            };
        }

        /// <summary>How many binders arm <paramref name="arm"/> pushes, in source order.</summary>
        internal static int ArmBinders(Term.Match match, int arm)
        {
            static int? InTree(DecisionTree tree, int arm) => tree switch
            {
                DecisionTree.Leaf leaf => leaf.Branch == arm ? leaf.Bindings.Length : null,
                DecisionTree.Switch sw => sw.Cases.Select(c => InTree(c.Tree, arm)).FirstOrDefault(n => n is not null) ?? InTree(sw.Default, arm),
                DecisionTree.Destruct d => d.Cases.Select(c => InTree(c.Tree, arm)).FirstOrDefault(n => n is not null)
                    ?? (d.Default is null ? null : InTree(d.Default, arm)),
                DecisionTree.TypeSwitch ts => ts.Cases.Select(c => InTree(c.Tree, arm)).FirstOrDefault(n => n is not null) ?? InTree(ts.Default, arm),
                DecisionTree.Sequential s => Binds(s.Arms[arm]),
                _ => null,
            };

            static int Binds(CorePattern p) => p switch
            {
                CorePattern.Bind => 1,
                CorePattern.Wild or CorePattern.Atom or CorePattern.AtomType => 0,
                CorePattern.Or or => Binds(or.Left),
                CorePattern.Prod prod => prod.Items.Sum(Binds),
                CorePattern.Con con => con.Args.Sum(Binds),
                CorePattern.Record r => r.Fields.Sum(f => Binds(f.Pattern)),
                CorePattern.StructType st => st.Fields.Sum(f => Binds(f.Pattern)),
                CorePattern.NominalHead h => h.Params.Sum(Binds),
                _ => throw new NotImplementedException($"not ported yet: the binders of a stuck {p.GetType().Name} arm"),
            };

            return InTree(match.Tree, arm)
                ?? throw new InvalidOperationException("reading back an unreachable arm of a stuck match");
        }

        /// <summary>A stuck match's arm body, evaluated with fresh variables for its binders from <paramref name="width"/>.</summary>
        internal static (Value Body, int Binders) OpenArm(MetaContext mc, int width, Frame.FMatch frame, int arm)
        {
            var binders = ArmBinders(frame.Match, arm);
            var env = Enumerable.Range(0, binders).Aggregate(frame.Env, (e, j) => e.Push(new Value.VVar(width + j, [])));
            return (Eval(mc, env, frame.Match.Bodies[arm]), binders);
        }

        private static Term QuoteStuckMatch(MetaContext mc, int width, Term scrutinee, Frame.FMatch frame) =>
            frame.Match with
            {
                Scrutinee = scrutinee,
                Bodies = [.. frame.Match.Bodies.Select((_, i) =>
                {
                    var (body, binders) = OpenArm(mc, width, frame, i);
                    return Quote(mc, width + binders, body);
                })],
            };
    }
}
