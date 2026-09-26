using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Nbe
{
    private abstract partial record Kont
    {
        /// <summary>The scrutinee is evaluated; choose an arm and run its result.</summary>
        public sealed record MatchOn(Environment Env, Term.Match Match) : Kont;
    }

    /// <summary>What a tree walk produced: a chosen arm, or the unknown value it is stuck on.</summary>
    private abstract record MatchStep
    {
        public sealed record Arm(Environment Env, Term Body) : MatchStep;

        /// <summary>The unknown value the match waits on: its head, or a part a pattern inspects.</summary>
        public sealed record Stuck(Value Value) : MatchStep;
    }

    /// <summary>The value an occurrence resolves to, or the unknown value the walk hit.</summary>
    private abstract record AtValue
    {
        public sealed record Resolved(Value Value) : AtValue;

        public sealed record Stuck(Value Value) : AtValue;
    }

    /// <summary>
    /// Walks the match's decision tree over the scrutinee and returns the chosen
    /// arm's result with its binders pushed in source order, or the unknown value a
    /// test inspects - the scrutinee's head, or a sub-position holding a variable -
    /// on which the match waits. The walk inspects values only; it evaluates
    /// nothing, so it runs no program.
    /// </summary>
    private static MatchStep SelectArm(MetaContext mc, Environment env, Value scrutinee, Term.Match match)
    {
        if (Force(mc, scrutinee) is (Value.VNeutral or Value.VVar or Value.VMeta) and var unknownHead)
            return new MatchStep.Stuck(unknownHead);

        var tree = match.Tree;
        while (true)
        {
            switch (tree)
            {
                case DecisionTree.Leaf leaf:
                {
                    var arm = leaf.Bindings.Aggregate(env, (e, at) => e.Push(Resolved(mc, scrutinee, at)));
                    return new MatchStep.Arm(arm, match.Bodies[leaf.Branch]);
                }

                case DecisionTree.Switch sw:
                {
                    var at = At(mc, scrutinee, sw.At);
                    if (at is AtValue.Stuck stuck) return new MatchStep.Stuck(stuck.Value);
                    var value = Force(mc, ((AtValue.Resolved)at).Value);
                    switch (value)
                    {
                        case Value.VAtom a:
                            tree = sw.Cases.FirstOrDefault(c => c.Key == a.Atom)?.Tree ?? sw.Default;
                            break;
                        case Value.VNeutral or Value.VVar or Value.VMeta:
                            return new MatchStep.Stuck(value);
                        default:
                            throw new InvalidOperationException($"a match reached a {value.GetType().Name} its type rules out");
                    }
                    continue;
                }

                case DecisionTree.Destruct destruct:
                {
                    var at = At(mc, scrutinee, destruct.At);
                    if (at is AtValue.Stuck stuck) return new MatchStep.Stuck(stuck.Value);
                    var value = Force(mc, ((AtValue.Resolved)at).Value);
                    switch (value)
                    {
                        case Value.VCon con:
                            tree = destruct.Cases.FirstOrDefault(c => c.Name == con.Name)?.Tree
                                ?? destruct.Default
                                ?? throw new InvalidOperationException($"no arm for constructor `{con.Name}`: the match was checked exhaustive");
                            break;
                        case Value.VNeutral or Value.VVar or Value.VMeta:
                            return new MatchStep.Stuck(value);
                        default:
                            throw new InvalidOperationException($"a match reached a {value.GetType().Name} its type rules out");
                    }
                    continue;
                }

                case DecisionTree.TypeSwitch typeSwitch:
                {
                    var at = At(mc, scrutinee, typeSwitch.At);
                    if (at is AtValue.Stuck stuck) return new MatchStep.Stuck(stuck.Value);
                    var ty = Force(mc, ((AtValue.Resolved)at).Value);
                    switch (ty)
                    {
                        case Value.VAtomTy a:
                            tree = typeSwitch.Cases.FirstOrDefault(c => c.Key is TypeKey.Atom k && k.Ty == a.Ty)?.Tree ?? typeSwitch.Default;
                            break;
                        case Value.VNeutral or Value.VVar or Value.VMeta:
                            return new MatchStep.Stuck(ty);
                        default:
                            if (typeSwitch.Cases.Any(c => c.Key is TypeKey.Nominal))
                                throw new InvalidOperationException("a nominal type-case ran as a tree");
                            tree = typeSwitch.Default;
                            break;
                    }
                    continue;
                }

                case DecisionTree.Sequential sequential:
                    return SelectArmInOrder(mc, env, scrutinee, match, sequential);

                default:
                    throw new InvalidOperationException($"unhandled decision tree {tree.GetType().Name}");
            }
        }
    }

    /// <summary>The value at a position inside a scrutinee, or the unknown value the walk hit.</summary>
    private static AtValue At(MetaContext mc, Value root, Occurrence at)
    {
        if (at is Occurrence.Base)
            return new AtValue.Resolved(root);

        var parent = at switch
        {
            Occurrence.Child c => c.Parent,
            Occurrence.Payload p => p.Parent,
            Occurrence.Field f => f.Parent,
            _ => throw new InvalidOperationException($"unhandled occurrence {at.GetType().Name}"),
        };

        var parentAt = At(mc, root, parent);
        if (parentAt is AtValue.Stuck stuck)
            return stuck;

        var parentValue = Force(mc, ((AtValue.Resolved)parentAt).Value);
        return at switch
        {
            Occurrence.Child c => parentValue switch
            {
                Value.VProd p => new AtValue.Resolved(p.Items[c.Index]),
                var other => StuckOrResolved(other),
            },
            Occurrence.Payload p => parentValue switch
            {
                Value.VCon con => new AtValue.Resolved(con.Args[p.Index]),
                var other => StuckOrResolved(other),
            },
            Occurrence.Field f => parentValue switch
            {
                Value.VRecord record => new AtValue.Resolved(record.Fields.Last(field => field.Name == f.Name).Value),
                var other => StuckOrResolved(other),
            },
            _ => throw new InvalidOperationException($"unhandled occurrence {at.GetType().Name}"),
        };
    }

    /// <summary>The value a binding occurrence names; an unknown one binds that unknown value.</summary>
    private static Value Resolved(MetaContext mc, Value root, Occurrence at) => At(mc, root, at) switch
    {
        AtValue.Resolved r => r.Value,
        AtValue.Stuck stuck => stuck.Value,
        _ => throw new InvalidOperationException("unhandled occurrence value"),
    };

    /// <summary>An unknown parent is stuck; any other shape is a type the tree rules out.</summary>
    private static AtValue StuckOrResolved(Value other) => other is Value.VNeutral or Value.VVar or Value.VMeta
        ? new AtValue.Stuck(other)
        : throw new InvalidOperationException($"a match reached a {other.GetType().Name} its type rules out");
}
