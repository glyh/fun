using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Elaborator
{
    /// <summary>
    /// Let-generalisation: a let-bound single-parameter lambda whose type still holds
    /// unsolved metas abstracts them as implicit type parameters, so each use
    /// instantiates them afresh (<c>f = fn(x) { x }</c> is usable at two types).
    /// Only where the lambda names nothing outside itself and no bound entry is in
    /// scope, as the prototype restricts it: wrapping the value in implicit lambdas
    /// would otherwise shift what it names.
    /// </summary>
    // The prototype quotes every implicit layer's body at the same depth, which only
    // lines up for one layer; here each unsolved meta becomes a binder level and the
    // type is read back once under all of them.
    private static (Term, Value) Generalise(Context ctx, Term value, Value type)
    {
        if (value is not Term.Lam { Body: var body } || body is Term.Lam || ctx.EntryKinds.Contains(EntryKind.Bound))
            return (value, type);

        var unsolved = new List<int>();
        CollectUnsolved(ctx, type, unsolved);
        // Closedness is decided only when there is something to generalise.
        if (unsolved.Count == 0 || !ClosedUnder(body, 1)) return (value, type);

        var n = unsolved.Count;
        for (var i = 0; i < n; i++) ctx.Metas.Solve(unsolved[i], new Value.VVar(ctx.Width + n - 1 - i, []));

        Term generalisedType = Nbe.Quote(ctx.Metas, ctx.Width + n, type);
        Term generalisedValue = value;
        for (var i = 0; i < n; i++)
        {
            generalisedType = new Term.Pi(Explicitness.Implicit, Term.U.Instance, generalisedType);
            generalisedValue = new Term.Lam(generalisedValue);
        }
        return (generalisedValue, ctx.Eval(generalisedType));
    }

    /// <summary>
    /// Whether <paramref name="term"/> names only entries it binds itself, plus
    /// <paramref name="depth"/> around it. A form the traversal does not know the
    /// binders of cannot be decided, which is "not ported yet", never "closed".
    /// </summary>
    private static bool ClosedUnder(Term term, int depth)
    {
        var closed = true;
        term.Map((t, under) =>
        {
            if (t is Term.Var v && v.Index >= under + depth) closed = false;
            return null;
        });
        return closed;
    }

    /// <summary>The unsolved metas a type mentions, outermost first, each once.</summary>
    private static void CollectUnsolved(Context ctx, Value type, List<int> seen)
    {
        var binder = new Value.VVar(ctx.Width, []);
        switch (ctx.Force(type))
        {
            case Value.VMeta { Spine.IsEmpty: true } m:
                if (!seen.Contains(m.Id)) seen.Add(m.Id);
                return;
            case Value.VMeta m:
                foreach (var a in m.Spine) CollectUnsolved(ctx, a, seen);
                return;
            case Value.VPi pi:
                CollectUnsolved(ctx, pi.Domain, seen);
                foreach (var e in Nbe.EvalRowClosure(ctx.Metas, pi.Row, binder).Effects) CollectUnsolved(ctx, e, seen);
                CollectUnsolved(ctx, Nbe.ApplyClosure(ctx.Metas, pi.Codomain, binder), seen);
                return;
            case Value.VRefTy r:
                CollectUnsolved(ctx, r.Heap, seen);
                CollectUnsolved(ctx, r.Element, seen);
                return;
        }
    }
}
