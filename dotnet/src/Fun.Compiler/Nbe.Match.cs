using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Nbe
{
    private abstract partial record Kont
    {
        /// <summary>The scrutinee is evaluated; choose an arm and run its result.</summary>
        public sealed record MatchOn(Environment Env, Term.Match Match) : Kont;
    }

    /// <summary>
    /// Walks the match's decision tree over the scrutinee and returns the chosen
    /// arm's result with its binders pushed in source order. The walk inspects
    /// values only; it evaluates nothing, so it runs no program.
    /// </summary>
    private static (Environment, Term) SelectArm(MetaContext mc, Environment env, Value scrutinee, Term.Match match)
    {
        var tree = match.Tree;
        while (true)
        {
            switch (tree)
            {
                case DecisionTree.Leaf leaf:
                    var arm = leaf.Bindings.Aggregate(env, (e, at) => e.Push(ValueAt(mc, scrutinee, at)));
                    return (arm, match.Bodies[leaf.Branch]);

                case DecisionTree.Switch sw:
                    tree = Force(mc, ValueAt(mc, scrutinee, sw.At)) switch
                    {
                        Value.VAtom a => sw.Cases.FirstOrDefault(c => c.Key == a.Atom)?.Tree ?? sw.Default,
                        var other => Stuck<DecisionTree>(other),
                    };
                    continue;

                case DecisionTree.Destruct:
                    throw new NotImplementedException("not ported yet: matching constructors");

                default:
                    throw new InvalidOperationException($"unhandled decision tree {tree.GetType().Name}");
            }
        }
    }

    /// <summary>The value at a position inside a scrutinee.</summary>
    private static Value ValueAt(MetaContext mc, Value root, Occurrence at) => at switch
    {
        Occurrence.Base => root,
        Occurrence.Child c => Force(mc, ValueAt(mc, root, c.Parent)) switch
        {
            Value.VProd p => p.Items[c.Index],
            var other => Stuck<Value>(other),
        },
        _ => throw new InvalidOperationException($"unhandled occurrence {at.GetType().Name}"),
    };

    /// <summary>
    /// A match whose tested value is not yet known - a variable or meta under the
    /// checker - would be stuck; stuck matches are not ported.
    /// </summary>
    private static T Stuck<T>(Value value) => value is Value.VNeutral or Value.VVar or Value.VMeta
        ? throw new NotImplementedException("not ported yet: a match stuck on an unknown value")
        : throw new InvalidOperationException($"a match reached a {value.GetType().Name} its type rules out");
}
