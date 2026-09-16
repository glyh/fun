using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Nbe
{
    /// <summary>
    /// A type-case switch on primitive types. A nominal case never reaches here:
    /// a match holding one runs its arms in order.
    /// </summary>
    private static DecisionTree SelectTypeCase(MetaContext mc, Value type, DecisionTree.TypeSwitch typeSwitch) => Force(mc, type) switch
    {
        Value.VAtomTy a => typeSwitch.Cases.FirstOrDefault(c => c.Key is TypeKey.Atom k && k.Ty == a.Ty)?.Tree ?? typeSwitch.Default,
        (Value.VNeutral or Value.VVar or Value.VMeta) and var other => Stuck<DecisionTree>(other),
        _ when typeSwitch.Cases.Any(c => c.Key is TypeKey.Nominal) =>
            throw new InvalidOperationException("a nominal type-case ran as a tree"),
        _ => typeSwitch.Default,
    };

    /// <summary>
    /// The first arm whose pattern matches, with its binders pushed in the order
    /// the arm writes them.
    /// </summary>
    private static (Environment, Term) SelectArmInOrder(MetaContext mc, Environment env, Value scrutinee, Term.Match match, DecisionTree.Sequential arms)
    {
        for (var i = 0; i < arms.Arms.Length; i++)
        {
            var binds = new List<(Occurrence At, Value Value)>();
            if (!Matches(mc, env, arms.Arms[i], scrutinee, Occurrence.Base.Instance, binds)) continue;
            var arm = MatchCompile.InSourceOrder(binds, b => b.At).Aggregate(env, (e, b) => e.Push(b.Value));
            return (arm, match.Bodies[i]);
        }
        throw new InvalidOperationException("no arm matched: the match was checked exhaustive");
    }

    /// <summary>Whether <paramref name="value"/> matches <paramref name="pattern"/>, collecting its binders by position.</summary>
    private static bool Matches(MetaContext mc, Environment env, CorePattern pattern, Value value, Occurrence at, List<(Occurrence, Value)> binds)
    {
        switch (pattern)
        {
            case CorePattern.Wild:
                return true;
            case CorePattern.Bind:
                binds.Add((at, value));
                return true;
            case CorePattern.Or or:
                var mark = binds.Count;
                if (Matches(mc, env, or.Left, value, at, binds)) return true;
                binds.RemoveRange(mark, binds.Count - mark);
                return Matches(mc, env, or.Right, value, at, binds);
        }

        switch (pattern, Force(mc, value))
        {
            case (CorePattern.Atom a, Value.VAtom v):
                return a.Value == v.Atom;
            case (CorePattern.AtomType t, Value.VAtomTy v):
                return t.Ty == v.Ty;
            case (CorePattern.Prod p, Value.VProd v) when p.Items.Length == v.Items.Length:
                return p.Items.Select((item, i) => (item, i)).All(x => Matches(mc, env, x.item, v.Items[x.i], new Occurrence.Child(at, x.i), binds));
            case (CorePattern.Con c, Value.VCon v):
                return c.Name == v.Name
                    && c.Args.Select((arg, i) => (arg, i)).All(x => Matches(mc, env, x.arg, v.Args[x.i], new Occurrence.Payload(at, c.Name, x.i), binds));
            case (CorePattern.Record r, Value.VRecord v):
                return r.Fields.All(f => Matches(mc, env, f.Pattern, v.Fields.Last(field => field.Name == f.Name).Value, new Occurrence.Field(at, f.Name), binds));
            case (CorePattern.StructType s, Value.VStruct v):
            {
                var fields = v.Entries.OfType<ModuleEntry.Field>().Where(f => f.Kind == MemberKind.Field).ToList();
                if (!s.Partial && fields.Count != s.Fields.Length) return false;
                return s.Fields.All(f => fields.LastOrDefault(field => field.Name == f.Name) is { } field
                    && Matches(mc, env, f.Pattern, field.Value, new Occurrence.Field(at, f.Name), binds));
            }
            case (CorePattern.NominalHead h, Value.VNominal v):
                return MatchesNominalHead(mc, env, h, v, at, binds);
            case (_, (Value.VNeutral or Value.VVar or Value.VMeta) and var stuck):
                return Stuck<bool>(stuck);
            default:
                return false;
        }
    }

    /// <summary>
    /// A type matches a nominal head when it is the instance the head names: the
    /// head, a type former applied to one fresh meta per parameter, is the same
    /// declaration over structurally equal captures, each meta standing for
    /// whatever it lines up with (E11). The metas then give the parameters.
    /// </summary>
    // ponytail: evaluating the head and applying a former nests one evaluation
    // per nominal-head match on the native stack (as Force does), not a Kont frame.
    private static bool MatchesNominalHead(MetaContext mc, Environment env, CorePattern.NominalHead head, Value.VNominal type, Occurrence at, List<(Occurrence, Value)> binds)
    {
        if (!ReferenceEquals(head.Decl, type.Decl)) return false;

        var metas = Enumerable.Range(0, head.Arity).Select(_ => mc.Fresh()).ToList();
        var written = metas.Aggregate(Eval(mc, env, head.Head), (f, id) => Apply(mc, f, new Value.VMeta(id, [])));
        var solved = new Dictionary<int, Value>();
        if (!SameInstance(mc, written, type, metas, solved)) return false;

        for (var i = 0; i < head.Arity; i++)
        {
            var parameter = solved.TryGetValue(metas[i], out var p) ? p : new Value.VMeta(metas[i], []);
            if (!Matches(mc, env, head.Params[i], parameter, new Occurrence.Child(at, i), binds)) return false;
        }
        return true;
    }

    /// <summary>
    /// Structural equality of run-time type values, a pattern's own metas matching
    /// anything once and the same thing every time after. Values with no
    /// structural identity - functions among them - are never equal.
    /// </summary>
    private static bool SameInstance(MetaContext mc, Value written, Value actual, List<int> metas, Dictionary<int, Value> solved)
    {
        written = Force(mc, written);
        actual = Force(mc, actual);
        switch (written, actual)
        {
            case (Value.VMeta { Spine.IsEmpty: true } m, _) when metas.Contains(m.Id):
                if (solved.TryGetValue(m.Id, out var previous)) return SameInstance(mc, previous, actual, [], solved);
                solved[m.Id] = actual;
                return true;
            case (Value.VNominal a, Value.VNominal b):
                return ReferenceEquals(a.Decl, b.Decl) && Pairwise(a.Captures, b.Captures);
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
            a.Length == b.Length && a.Zip(b).All(p => SameInstance(mc, p.First, p.Second, metas, solved));
    }
}
