using Quill.Kernel;

namespace Quill.Compiler;

/// <summary>
/// E11's run-time half: whether a type is the instance a type-case head names.
/// A nominal is the same type when it is the same declaration over the same
/// captures. Captures are compared by identity where they have a fixed reading
/// - a stamp is a cell - and a capture that is a function is compared the way
/// conversion compares it (eta-applied to a fresh variable), which is what
/// separates two evaluations of a generative module (each mints its own cell)
/// while sharing one evaluation's type with itself.
/// </summary>
public static partial class Nbe
{
    /// <summary>
    /// A type matches a nominal head when it is the instance the head names: the
    /// head, a type former applied to one fresh meta per parameter, is the same
    /// declaration over the same captures (E11). The metas then give the parameters.
    /// </summary>
    // ponytail: evaluating the head and applying a former nests one evaluation
    // per nominal-head match on the native stack (as Force does), not a Kont frame - a
    // shared choice documented for uniformity, with no observable difference.
    private static MatchResult MatchesNominalHead(MetaContext mc, Environment env, CorePattern.NominalHead head, Value.VNominal type, Occurrence at, List<(Occurrence, Value)> binds)
    {
        if (!ReferenceEquals(head.Decl, type.Decl)) return new MatchResult.NoMatch();

        var metas = Enumerable.Range(0, head.Arity).Select(_ => mc.Fresh()).ToList();
        // The head is level-addressed: its indices are rooted at the width it was
        // elaborated at, so re-root it onto this environment by the difference -
        // run-time environments are bottom-aligned with elaboration contexts - and
        // it resolves where it was written, not where the match sits.
        var rooted = head.HeadWidth is 0 ? head.Head : head.Head.Shift(env.Count - head.HeadWidth);
        var written = metas.Aggregate(Eval(mc, env, rooted), (f, id) => Apply(mc, f, new Value.VMeta(id, [])));
        var solved = new Dictionary<int, Value>();
        if (!SameInstance(mc, env.Count, written, type, metas, solved)) return new MatchResult.NoMatch();

        for (var i = 0; i < head.Arity; i++)
        {
            var parameter = solved.TryGetValue(metas[i], out var p) ? p : new Value.VMeta(metas[i], []);
            var sub = Matches(mc, env, head.Params[i], parameter, new Occurrence.Child(at, i), binds);
            if (sub is not MatchResult.Matched) return sub;
        }
        return new MatchResult.Matched();
    }

    /// <summary>
    /// Structural equality of run-time type values, a pattern's own metas matching
    /// anything once and the same thing every time after. Equal when the same
    /// object, a cell by identity (a stamp), a function the way conversion compares
    /// it (eta-applied to a fresh variable), else shape by shape.
    /// </summary>
    private static bool SameInstance(MetaContext mc, int width, Value written, Value actual, List<int> metas, Dictionary<int, Value> solved)
    {
        if (ReferenceEquals(written, actual)) return true;
        written = Force(mc, written);
        actual = Force(mc, actual);
        if (ReferenceEquals(written, actual)) return true;
        switch (written, actual)
        {
            case (Value.VMeta { Spine.IsEmpty: true } m, _) when metas.Contains(m.Id):
                if (solved.TryGetValue(m.Id, out var previous)) return SameInstance(mc, width, previous, actual, [], solved);
                solved[m.Id] = actual;
                return true;
            // A closure capture is compared the way conversion compares it (Unify's
            // eta arm): apply the candidate's body and the written side to one fresh
            // variable and compare the results, so two capture lambdas agree when they
            // agree on every argument. The results are neutrals for a stuck body, so
            // they are compared by readback (Convertible), the same comparison effect
            // instances use, rather than by SameInstance, which has no neutral arm.
            case (_, Value.VLam b):
            {
                var probe = new Value.VVar(width, []);
                return Convertible(mc, width + 1, Apply(mc, written, probe), ApplyClosure(mc, b.Body, probe));
            }
            case (Value.VNominal a, Value.VNominal b):
                return ReferenceEquals(a.Decl, b.Decl) && Pairwise(a.Captures, b.Captures);
            case (Value.VRef a, Value.VRef b):
                return ReferenceEquals(a.Cell, b.Cell);
            case (Value.VEffect a, Value.VEffect b):
                return a.Family.Id == b.Family.Id && Pairwise(a.Params, b.Params);
            case (Value.VAtom a, Value.VAtom b):
                return a.Atom == b.Atom;
            case (Value.VAtomTy a, Value.VAtomTy b):
                return a.Ty == b.Ty;
            case (Value.VU, Value.VU):
                return true;
            case (Value.VProdTy a, Value.VProdTy b):
                return Pairwise(a.Items, b.Items);
            case (Value.VProd a, Value.VProd b):
                return Pairwise(a.Items, b.Items);
            default:
                return false;
        }

        bool Pairwise(EquatableArray<Value> a, EquatableArray<Value> b) =>
            a.Length == b.Length && a.Zip(b).All(p => SameInstance(mc, width, p.First, p.Second, metas, solved));
    }
}
