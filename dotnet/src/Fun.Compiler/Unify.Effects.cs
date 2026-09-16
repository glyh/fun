using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Unify
{
    /// <summary>Two effect instances: the same family, and parameters that unify (E1).</summary>
    private static void Effects(MetaContext mc, int width, Value.VEffect left, Value.VEffect right)
    {
        if (left.Family.Id != right.Family.Id)
            throw new UnifyException($"effect {left.Family.Name} is not effect {right.Family.Name}");
        Pairwise(mc, width, left.Params, right.Params);
    }

    /// <summary>
    /// Two rows, as sets (E2). Each side's effects the other side names cancel, as
    /// do the tails both sides name; what one side has left goes to the other
    /// side's single remaining tail. A union of two unsolved tails against something
    /// concrete has no principal solution and is a mismatch rather than a guess.
    /// </summary>
    public static void Rows(MetaContext mc, int width, Value.VEffectRow left, Value.VEffectRow right)
    {
        left = Nbe.NormalizeRow(mc, left.Effects, left.Tails);
        right = Nbe.NormalizeRow(mc, right.Effects, right.Tails);

        var rightEffects = right.Effects.ToList();
        var leftEffects = new List<Value>();
        foreach (var effect in left.Effects)
        {
            var match = rightEffects.FindIndex(candidate => TryValues(mc, width, effect, candidate));
            if (match >= 0) rightEffects.RemoveAt(match);
            else leftEffects.Add(effect);
        }

        var rightTails = right.Tails.ToList();
        var leftTails = new List<Value>();
        foreach (var tail in left.Tails)
        {
            var match = rightTails.FindIndex(candidate => Nbe.Convertible(mc, width, tail, candidate));
            if (match >= 0) rightTails.RemoveAt(match);
            else leftTails.Add(tail);
        }

        Value.VEffectRow Row(List<Value> effects, List<Value> tails) => new([.. effects], [.. tails]);
        switch (leftEffects.Count, rightEffects.Count, leftTails.Count, rightTails.Count)
        {
            case (0, 0, 0, 0):
                return;
            case (0, _, 1, _):
                Values(mc, width, leftTails[0], Row(rightEffects, rightTails));
                return;
            case (_, 0, _, 1):
                Values(mc, width, Row(leftEffects, leftTails), rightTails[0]);
                return;
            default:
                throw new UnifyException("effect rows do not agree");
        }
    }

    /// <summary>Unifies two values, undoing every meta it solved when it fails.</summary>
    public static bool TryValues(MetaContext mc, int width, Value left, Value right)
    {
        var snapshot = mc.Snapshot();
        try
        {
            Values(mc, width, left, right);
            return true;
        }
        catch (UnifyException)
        {
            mc.Restore(snapshot);
            return false;
        }
    }

    /// <summary>Two arrows' rows, each read under a common binder.</summary>
    private static void ArrowRows(MetaContext mc, int width, Value.VPi left, Value.VPi right)
    {
        if (left.Row.Row.IsPure && right.Row.Row.IsPure) return;
        var binder = new Value.VVar(width, []);
        Rows(mc, width + 1, Nbe.EvalRowClosure(mc, left.Row, binder), Nbe.EvalRowClosure(mc, right.Row, binder));
    }

    private static RowTerm RenameRow(MetaContext mc, int id, Renaming ren, Value.VEffectRow row) =>
        new([.. row.Effects.Select(e => Rename(mc, id, ren, e))], [.. row.Tails.Select(t => Rename(mc, id, ren, t))]);

    private static Term? RenameEffects(MetaContext mc, int id, Renaming ren, Value value) => value switch
    {
        Value.VEffectRowTy => Term.EffectRowTy.Instance,
        Value.VEffectRow row => new Term.EffectRowLit(RenameRow(mc, id, ren, row)),
        Value.VEffect e => new Term.Effect(e.Family, e.Environment, [.. e.Params.Select(p => Rename(mc, id, ren, p))]),
        _ => null,
    };
}
