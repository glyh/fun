using System.Collections.Immutable;
using Fun.Kernel;

namespace Fun.Compiler;

/// <summary>Two types that do not unify.</summary>
public sealed class UnifyException(string message) : Exception(message);

/// <summary>Structural unification of values, solving metas as it goes.</summary>
public static partial class Unify
{
    public static void Values(MetaContext mc, int width, Value left, Value right)
    {
        mc.Budget.Spend("a unification");
        if (SameDeferredCall(mc, width, left, right)) return;
        left = Nbe.Force(mc, left);
        right = Nbe.Force(mc, right);
        var fresh = new Value.VVar(width, []);

        switch (left, right)
        {
            case (Value.VU, Value.VU):
                return;
            case (Value.VAtomTy a, Value.VAtomTy b) when a.Ty == b.Ty:
                return;
            case (Value.VAtom a, Value.VAtom b) when a.Atom == b.Atom:
                return;

            case (Value.VEffectRowTy, Value.VEffectRowTy): return;
            case (Value.VEffectRow a, Value.VEffectRow b): Rows(mc, width, a, b); return;
            case (Value.VEffect a, Value.VEffect b): Effects(mc, width, a, b); return;

            case (Value.VPi a, Value.VPi b) when a.Explicitness == b.Explicitness:
                Values(mc, width, a.Domain, b.Domain);
                ArrowRows(mc, width, a, b);
                Values(mc, width + 1,
                    Nbe.ApplyClosure(mc, a.Codomain, fresh), Nbe.ApplyClosure(mc, b.Codomain, fresh));
                return;

            case (Value.VLam a, Value.VLam b):
                Values(mc, width + 1, Nbe.ApplyClosure(mc, a.Body, fresh), Nbe.ApplyClosure(mc, b.Body, fresh));
                return;

            // Eta: a function equals a lambda when they agree on a fresh argument.
            case (Value.VLam a, _):
                Values(mc, width + 1, Nbe.ApplyClosure(mc, a.Body, fresh), Nbe.Apply(mc, right, fresh));
                return;
            case (_, Value.VLam b):
                Values(mc, width + 1, Nbe.Apply(mc, left, fresh), Nbe.ApplyClosure(mc, b.Body, fresh));
                return;

            case (Value.VProd a, Value.VProd b):
                Pairwise(mc, width, a.Items, b.Items);
                return;
            case (Value.VProdTy a, Value.VProdTy b):
                Pairwise(mc, width, a.Items, b.Items);
                return;

            case (Value.VVar a, Value.VVar b) when a.Level == b.Level:
                Pairwise(mc, width, a.Spine, b.Spine);
                return;

            case (Value.VMeta a, Value.VMeta b) when a.Id == b.Id:
                Pairwise(mc, width, a.Spine, b.Spine);
                return;
            case (Value.VMeta a, _):
                Solve(mc, width, a.Id, a.Spine, right);
                return;
            case (_, Value.VMeta b):
                Solve(mc, width, b.Id, b.Spine, left);
                return;

            case (Value.VRecursiveOccurrence, _) or (_, Value.VRecursiveOccurrence): RecursiveOccurrences(mc, width, left, right); return;
            case (Value.VNominal or Value.VCon, Value.VNominal or Value.VCon): UnifyNominals(mc, width, left, right); return;

            case (Value.VSig a, Value.VSig b): Signatures(mc, width, a, b); return;
            case (Value.VModule a, Value.VModule b): Modules(mc, width, a, b); ModuleImpls(mc, width, a, b); return;
            case (Value.VTrait or Value.VTraitDict, Value.VTrait or Value.VTraitDict): UnifyTraits(mc, width, left, right); return;
            case (Value.VStruct a, Value.VStruct b): Structs(mc, width, a, b); return;
            case (Value.VRecord a, Value.VRecord b): Records(mc, width, a, b); return;

            case (Value.VNeutral a, Value.VNeutral b): Neutrals(mc, width, a, b); return;

            case (Value.VFix a, Value.VFix b):
                FixBodies(mc, width, a, b);
                return;

            default:
                throw new UnifyException($"cannot unify {left.GetType().Name} with {right.GetType().Name}");
        }
    }

    private static void Pairwise(MetaContext mc, int width, EquatableArray<Value> a, EquatableArray<Value> b)
    {
        if (a.Length != b.Length) throw new UnifyException("length mismatch");
        for (var i = 0; i < a.Length; i++) Values(mc, width, a[i], b[i]);
    }

    /// <summary>
    /// Solves <c>?id[spine] = rhs</c> at <paramref name="width"/> entries. With an
    /// empty spine the meta abstracts over nothing, so its solution is the value
    /// itself, after an occurs check. Otherwise the spine must be distinct bound
    /// variables, and the solution is <c>rhs</c> abstracted over them.
    /// </summary>
    private static void Solve(MetaContext mc, int width, int id, EquatableArray<Value> spine, Value rhs)
    {
        if (spine.IsEmpty)
        {
            OccursCheck(mc, id, rhs);
            mc.Solve(id, rhs);
            return;
        }

        var renaming = Invert(mc, width, spine);
        Term body = Rename(mc, id, renaming, rhs);
        for (var i = 0; i < spine.Length; i++) body = new Term.Lam(body);
        mc.Solve(id, Nbe.Eval(mc, Environment.Empty, body));
    }

    /// <summary>
    /// A partial renaming from the context a meta is solved in (<c>Cod</c>
    /// entries) to its solution's lambdas (<c>Dom</c> entries): which context
    /// level each solution level stands for.
    /// </summary>
    private sealed record Renaming(int Dom, int Cod, ImmutableDictionary<int, int> Levels)
    {
        /// <summary>
        /// Under a binder in the right-hand side: the binder is a new variable on
        /// both sides, so the renaming grows by it. Not lifting here is the
        /// prototype's defect (meta-solution-renaming-not-lifted-under-binders).
        /// </summary>
        public Renaming Lift() => new(Dom + 1, Cod + 1, Levels.SetItem(Cod, Dom));
    }

    /// <summary>The renaming a pattern spine denotes: each argument a distinct bound variable.</summary>
    private static Renaming Invert(MetaContext mc, int width, EquatableArray<Value> spine)
    {
        var levels = ImmutableDictionary<int, int>.Empty;
        for (var i = 0; i < spine.Length; i++)
        {
            if (Nbe.Force(mc, spine[i]) is not Value.VVar { Spine.IsEmpty: true } variable)
                throw new UnifyException("a meta's spine argument is not a variable");
            if (levels.ContainsKey(variable.Level))
                throw new UnifyException("a meta's spine repeats a variable");
            levels = levels.Add(variable.Level, i);
        }
        return new Renaming(spine.Length, width, levels);
    }

    /// <summary>
    /// Reads <paramref name="value"/> back as a term over the solution's lambdas,
    /// failing on a variable the spine does not abstract (it would escape) and on
    /// the meta being solved (an infinite solution).
    /// </summary>
    private static Term Rename(MetaContext mc, int id, Renaming ren, Value value)
    {
        Term Go(Value v) => Rename(mc, id, ren, v);
        Term Var(int level) => ren.Levels.TryGetValue(level, out var target)
            ? new Term.Var(Nbe.LevelToIndex(ren.Dom, target))
            : throw new UnifyException("a variable outside the meta's spine escapes into its solution");
        Term Spine(Term head, EquatableArray<Value> spine) =>
            spine.Aggregate(head, (acc, a) => new Term.Ap(acc, Explicitness.Explicit, Go(a)));
        var fresh = new Value.VVar(ren.Cod, []);

        return Nbe.Force(mc, value) switch
        {
            Value.VMeta f when f.Id == id => throw new UnifyException("occurs check: a meta in its own solution"),
            Value.VMeta f => Spine(new Term.Meta(f.Id), f.Spine),
            Value.VVar r => Spine(Var(r.Level), r.Spine),
            Value.VLam lam => new Term.Lam(Rename(mc, id, ren.Lift(), Nbe.ApplyClosure(mc, lam.Body, fresh))),
            Value.VPi pi => new Term.Pi(pi.Explicitness, Go(pi.Domain),
                Rename(mc, id, ren.Lift(), Nbe.ApplyClosure(mc, pi.Codomain, fresh)))
            {
                Row = pi.Row.Row.IsPure ? RowTerm.Pure : RenameRow(mc, id, ren.Lift(), Nbe.EvalRowClosure(mc, pi.Row, fresh)),
            },
            Value.VEffectRowTy or Value.VEffectRow or Value.VEffect => RenameEffects(mc, id, ren, value)!,
            Value.VU => Term.U.Instance,
            Value.VAtom a => new Term.Atom(a.Atom),
            Value.VAtomTy a => new Term.AtomTy(a.Ty),
            Value.VProd p => new Term.Prod([.. p.Items.Select(Go)]),
            Value.VProdTy p => new Term.ProdTy([.. p.Items.Select(Go)]),
            Value.VNominal n => new Term.Nominal(n.Decl, [.. n.Captures.Select(Go)]),
            Value.VRecursiveOccurrence o => new Term.RecursiveOccurrence(o.Decl, [.. o.Captures.Select(Go)], [.. o.Args.Select(Go)]),
            Value.VCon c => c.Args.Aggregate((Term)new Term.Dot(Go(c.Nominal), c.Name), (acc, a) => new Term.Ap(acc, Explicitness.Explicit, Go(a))),
            Value.VNeutral n => n.Frames.Aggregate(n.Head switch
            {
                Head.HVar h => Var(h.Level),
                Head.HMeta h when h.Id == id => throw new UnifyException("occurs check: a meta in its own solution"),
                Head.HMeta h => new Term.Meta(h.Id),
                Head.HPrim h => new Term.Prim(h.Name),
                _ => throw new InvalidOperationException($"unhandled head {n.Head.GetType().Name}"),
            }, (acc, frame) => frame switch
            {
                Frame.FApp a => new Term.Ap(acc, Explicitness.Explicit, Go(a.Arg)),
                Frame.FProj p => new Term.Proj(acc, p.Index),
                _ => throw new InvalidOperationException($"unhandled frame {frame.GetType().Name}"),
            }),
            var other => throw new NotImplementedException($"not ported yet: solving to {other.GetType().Name}"),
        };
    }

    private static void OccursCheck(MetaContext mc, int id, Value value)
    {
        switch (Nbe.Force(mc, value))
        {
            case Value.VMeta f:
                if (f.Id == id) throw new UnifyException("occurs check: a meta in its own solution");
                foreach (var v in f.Spine) OccursCheck(mc, id, v);
                return;
            case Value.VPi pi:
                OccursCheck(mc, id, pi.Domain);
                OccursCheck(mc, id, Nbe.ApplyClosure(mc, pi.Codomain, new Value.VVar(0, [])));
                return;
            case Value.VProd p:
                foreach (var v in p.Items) OccursCheck(mc, id, v);
                return;
            case Value.VProdTy p:
                foreach (var v in p.Items) OccursCheck(mc, id, v);
                return;
            case Value.VNominal n:
                foreach (var c in n.Captures) OccursCheck(mc, id, c);
                return;
            case Value.VEffectRow row:
                foreach (var v in row.Effects.Concat(row.Tails)) OccursCheck(mc, id, v);
                return;
            case Value.VEffect e:
                foreach (var p in e.Params) OccursCheck(mc, id, p);
                return;
            case Value.VRecursiveOccurrence o:
                foreach (var v in o.Captures.Concat(o.Args)) OccursCheck(mc, id, v);
                return;
            case Value.VNeutral n:
                foreach (var frame in n.Frames)
                    if (frame is Frame.FApp app) OccursCheck(mc, id, app.Arg);
                return;
            case Value.VModule or Value.VStruct or Value.VRecord or Value.VSig:
                foreach (var v in Contents(mc, value)) OccursCheck(mc, id, v);
                return;
            case Value.VTraitDict dict:
                foreach (var v in TraitContents(dict)) OccursCheck(mc, id, v);
                return;
            // A bound variable, a lambda, a universe or an atom holds no meta
            // the check follows (the prototype does not look inside a lambda).
            default:
                return;
        }
    }
}
