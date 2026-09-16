using System.Collections.Immutable;
using Fun.Kernel;

namespace Fun.Compiler;

/// <summary>Two types that do not unify.</summary>
public sealed class UnifyException(string message) : Exception(message);

/// <summary>Structural unification of values, solving metavariables as it goes.</summary>
public static class Unify
{
    public static void Values(MetaContext mc, int depth, Value left, Value right)
    {
        left = Nbe.Force(mc, left);
        right = Nbe.Force(mc, right);
        var fresh = new Value.VRigid(depth, []);

        switch (left, right)
        {
            case (Value.VU, Value.VU):
                return;
            case (Value.VAtomTy a, Value.VAtomTy b) when a.Ty == b.Ty:
                return;
            case (Value.VAtom a, Value.VAtom b) when a.Atom == b.Atom:
                return;

            case (Value.VPi a, Value.VPi b) when a.Explicitness == b.Explicitness:
                Values(mc, depth, a.Domain, b.Domain);
                Values(mc, depth + 1,
                    Nbe.ApplyClosure(mc, a.Codomain, fresh), Nbe.ApplyClosure(mc, b.Codomain, fresh));
                return;

            case (Value.VLam a, Value.VLam b):
                Values(mc, depth + 1, Nbe.ApplyClosure(mc, a.Body, fresh), Nbe.ApplyClosure(mc, b.Body, fresh));
                return;

            // Eta: a function equals a lambda when they agree on a fresh argument.
            case (Value.VLam a, _):
                Values(mc, depth + 1, Nbe.ApplyClosure(mc, a.Body, fresh), Nbe.Apply(mc, right, fresh));
                return;
            case (_, Value.VLam b):
                Values(mc, depth + 1, Nbe.Apply(mc, left, fresh), Nbe.ApplyClosure(mc, b.Body, fresh));
                return;

            case (Value.VProd a, Value.VProd b):
                Pairwise(mc, depth, a.Items, b.Items);
                return;
            case (Value.VProdTy a, Value.VProdTy b):
                Pairwise(mc, depth, a.Items, b.Items);
                return;

            case (Value.VRigid a, Value.VRigid b) when a.Level == b.Level:
                Pairwise(mc, depth, a.Spine, b.Spine);
                return;

            case (Value.VFlex a, Value.VFlex b) when a.Id == b.Id:
                Pairwise(mc, depth, a.Spine, b.Spine);
                return;
            case (Value.VFlex a, _):
                Solve(mc, a.Id, a.Spine, right);
                return;
            case (_, Value.VFlex b):
                Solve(mc, b.Id, b.Spine, left);
                return;

            default:
                throw new UnifyException($"cannot unify {left.GetType().Name} with {right.GetType().Name}");
        }
    }

    private static void Pairwise(MetaContext mc, int depth, ImmutableArray<Value> a, ImmutableArray<Value> b)
    {
        if (a.Length != b.Length) throw new UnifyException("length mismatch");
        for (var i = 0; i < a.Length; i++) Values(mc, depth, a[i], b[i]);
    }

    /// <summary>
    /// Solves <c>?id[spine] = rhs</c>. With an empty spine the solution is the
    /// value itself, after an occurs check -- as the prototype does.
    /// </summary>
    private static void Solve(MetaContext mc, int id, ImmutableArray<Value> spine, Value rhs)
    {
        // The prototype's renaming for a non-empty spine reuses the solution's
        // own levels as keys into the renaming under a binder, so a dependent
        // right-hand side can be misrenamed. Not ported until that is settled.
        if (!spine.IsEmpty)
            throw new NotImplementedException("not ported yet: solving a metavariable applied to a spine");
        OccursCheck(mc, id, rhs);
        mc.Solve(id, rhs);
    }

    private static void OccursCheck(MetaContext mc, int id, Value value)
    {
        switch (Nbe.Force(mc, value))
        {
            case Value.VFlex f:
                if (f.Id == id) throw new UnifyException("occurs check: a metavariable in its own solution");
                foreach (var v in f.Spine) OccursCheck(mc, id, v);
                return;
            case Value.VPi pi:
                OccursCheck(mc, id, pi.Domain);
                OccursCheck(mc, id, Nbe.ApplyClosure(mc, pi.Codomain, new Value.VRigid(0, [])));
                return;
            case Value.VProd p:
                foreach (var v in p.Items) OccursCheck(mc, id, v);
                return;
            case Value.VProdTy p:
                foreach (var v in p.Items) OccursCheck(mc, id, v);
                return;
            case Value.VNeutral n:
                foreach (var frame in n.Frames)
                    if (frame is Frame.FApp app) OccursCheck(mc, id, app.Arg);
                return;
            // A rigid variable, a lambda, a universe or an atom holds no meta
            // the check follows (the prototype does not look inside a lambda).
            default:
                return;
        }
    }
}
