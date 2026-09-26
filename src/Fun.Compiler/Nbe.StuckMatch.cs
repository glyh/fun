using Fun.Kernel;

namespace Fun.Compiler
{
    public static partial class Nbe
    {
        /// <summary>
        /// A match waits as the unknown value's last frame, whether that value is the
        /// scrutinee's head or only a part a pattern inspects: the frame hangs on the
        /// unknown value, and <see cref="Frame.FMatch.Scrutinee"/> holds the whole
        /// scrutinee so read-back rebuilds it.
        /// </summary>
        private static Value StuckNeutral(Value stuck, Frame.FMatch frame) => stuck switch
        {
            Value.VNeutral n => n with { Ty = Value.VU.Instance, Frames = n.Frames.Add(frame) },
            Value.VVar v => new Value.VNeutral(Value.VU.Instance, new Head.HVar(v.Level), Spine(v.Spine).Add(frame)),
            Value.VMeta m => new Value.VNeutral(Value.VU.Instance, new Head.HMeta(m.Id), Spine(m.Spine).Add(frame)),
            _ => throw new InvalidOperationException($"a match stuck on a {stuck.GetType().Name} its type rules out"),
        };

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
                // The one statement of a pattern's binder count (CorePattern.Binders).
                DecisionTree.Sequential s => s.Arms[arm].Binders(),
                _ => null,
            };

            // A pruned arm (an earlier arm subsumes it) has no leaf to read, but its
            // pattern still says how many binders its body closes over.
            return InTree(match.Tree, arm)
                ?? (arm < match.Patterns.Length
                    ? match.Patterns[arm].Binders()
                    : throw new InvalidOperationException("reading back an unreachable arm of a stuck match without patterns"));
        }

        /// <summary>A stuck match's arm body, evaluated with fresh variables for its binders from <paramref name="width"/>.</summary>
        internal static (Value Body, int Binders) OpenArm(MetaContext mc, int width, Frame.FMatch frame, int arm)
        {
            var binders = ArmBinders(frame.Match, arm);
            var env = Enumerable.Range(0, binders).Aggregate(frame.Env, (e, j) => e.Push(new Value.VVar(width + j, [])));
            return (Eval(mc, env, frame.Match.Bodies[arm]), binders);
        }

        private static Term QuoteStuckMatch(MetaContext mc, int width, Frame.FMatch frame) =>
            frame.Match with
            {
                Scrutinee = Quote(mc, width, frame.Scrutinee),
                Bodies = [.. frame.Match.Bodies.Select((_, i) =>
                {
                    var (body, binders) = OpenArm(mc, width, frame, i);
                    return Quote(mc, width + binders, body);
                })],
            };
    }
}
