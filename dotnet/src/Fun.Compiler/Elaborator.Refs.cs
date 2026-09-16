using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Elaborator
{
    /// <summary>
    /// The base context's reference entries: <c>Ref : [h : Type] -&gt; Type -&gt; Type</c>,
    /// whose heap is never written (each <c>Ref(A)</c> gets a fresh one), and
    /// <c>Mutate : [h : Type] -&gt; [A : Type] -&gt; Ref(h, A) -&gt; Type</c>, mapping a
    /// reference to the effect on its heap.
    /// </summary>
    private static Context DefineReferenceEntries(Context ctx)
    {
        Term Pi(Explicitness e, Term domain, Term codomain) => new Term.Pi(e, domain, codomain);
        var type = Term.U.Instance;

        ctx = ctx.Define("Ref", ctx.Eval(Pi(Explicitness.Implicit, type, Pi(Explicitness.Explicit, type, type))),
            ctx.Eval(new Term.Lam(new Term.Lam(new Term.RefTy(new Term.Var(1), new Term.Var(0))))));

        var mutateType = Pi(Explicitness.Implicit, type, Pi(Explicitness.Implicit, type,
            Pi(Explicitness.Explicit, new Term.RefTy(new Term.Var(1), new Term.Var(0)), type)));
        var mutate = new Term.Lam(new Term.Lam(new Term.Lam(
            new Term.Effect(MutationEffect.Family, Environment.Empty, [new Term.Var(2)]))));
        return ctx.Define("Mutate", ctx.Eval(mutateType), ctx.Eval(mutate));
    }

    /// <summary>
    /// <c>ref</c>, <c>deref</c> and <c>&lt;-</c>. Each performs <c>Mutate</c> on the
    /// reference's heap; a new reference starts a heap of its own; a store is
    /// recorded for E6.
    /// </summary>
    private static (Term, Value) InferRefs(Context ctx, Syntax stx)
    {
        switch (stx)
        {
            case Syntax.RefNew n:
            {
                var (arg, type) = Infer(ctx, n.Arg);
                var heap = ctx.RawMeta();
                Emit(ctx, [MutationEffect.On(heap)], []);
                return (new Term.RefNew(arg), new Value.VRefTy(heap, type));
            }

            case Syntax.RefGet g:
            {
                var (reference, refType) = InferReference(ctx, g.Ref);
                Emit(ctx, [MutationEffect.On(refType.Heap)], []);
                return (new Term.RefGet(reference), ctx.Force(refType.Element));
            }

            case Syntax.RefSet s:
            {
                var (reference, refType) = InferReference(ctx, s.Ref);
                var value = Check(ctx, s.Value, refType.Element);
                ctx.Sink.Stored.Add((refType.Heap, refType.Element));
                Emit(ctx, [MutationEffect.On(refType.Heap)], []);
                return (new Term.RefSet(reference, value), new Value.VAtomTy(AtomTy.Unit));
            }

            default:
                throw new InvalidOperationException($"not a reference form: {stx.GetType().Name}");
        }
    }

    /// <summary>An expression used as a reference: its type is a reference type, or is made one when still unknown.</summary>
    private static (Term, Value.VRefTy) InferReference(Context ctx, Syntax stx)
    {
        var (term, type) = Infer(ctx, stx);
        (term, type) = InsertImplicitArgs(ctx, term, type);
        switch (ctx.Force(type))
        {
            case Value.VRefTy r:
                return (term, r);
            case Value.VMeta:
                var fresh = new Value.VRefTy(ctx.RawMeta(), ctx.RawMeta());
                ctx.Unify(type, fresh);
                return (term, fresh);
            default:
                throw new FunException("a reference operation on a value that is not a reference");
        }
    }

    // ---- discharge ------------------------------------------------------------

    /// <summary>
    /// A binding form whose heaps, allocated while elaborating it, its result type
    /// does not mention: their effects are dropped from what it performs, as at a
    /// function boundary. A <c>let</c> or block with private mutation is pure.
    /// </summary>
    private static (Term, Value) Discharging(Context ctx, Func<Context, (Term, Value)> elaborate)
    {
        var since = ctx.Metas.Count;
        var ((term, type), performed) = Collecting(ctx, elaborate);
        Emit(ctx, DischargeLocalHeaps(ctx, since, [type], performed));
        return (term, type);
    }

    /// <summary>
    /// <paramref name="performed"/> without the <c>Mutate</c> effects on heaps that
    /// are local - allocated since meta <paramref name="since"/> and not aliased by an
    /// older heap - and that nothing <paramref name="visible"/> mentions.
    /// </summary>
    private static EffectSink DischargeLocalHeaps(Context ctx, int since, IEnumerable<Value> visible, EffectSink performed)
    {
        var local = LocalHeaps(ctx, since);
        var seen = visible.ToList();
        var kept = new EffectSink();
        kept.Effects.AddRange(performed.Effects.Where(e =>
            MutationEffect.HeapOf(ctx.Force(e)) is not { } heap
            || local(heap) is not int id
            || seen.Any(v => Unify.Mentions(ctx.Metas, id, v))));
        kept.Tails.AddRange(performed.Tails);
        return kept;
    }

    /// <summary>
    /// Which heap a value is, when it is local: allocated since meta
    /// <paramref name="since"/>, and no older meta has been solved to it - an older
    /// heap solved to a newer id makes that id stand for the older heap too.
    /// </summary>
    // ponytail: one pass over the older metas per discharge site, as the prototype;
    // index aliases on the meta context if large programs make it show.
    private static Func<Value, int?> LocalHeaps(Context ctx, int since)
    {
        var aliased = new HashSet<int>();
        for (var i = 0; i < since; i++)
            if (ctx.Metas.Solution(i) is not null
                && Nbe.Force(ctx.Metas, new Value.VMeta(i, [])) is Value.VMeta { Spine.IsEmpty: true } m)
                aliased.Add(m.Id);
        return heap => Nbe.Force(ctx.Metas, heap) is Value.VMeta { Spine.IsEmpty: true } m && m.Id >= since && !aliased.Contains(m.Id)
            ? m.Id
            : null;
    }

    /// <summary>What an entry performs, less <c>Mutate</c> on any heap: the runtime's handler discharges those.</summary>
    private static EffectSink WithoutMutation(EffectSink performed)
    {
        var rest = new EffectSink();
        rest.Effects.AddRange(performed.Effects.Where(e => MutationEffect.HeapOf(e) is null));
        rest.Tails.AddRange(performed.Tails);
        return rest;
    }

    /// <summary>An effect as an error names it: <c>Mutate</c> names a reference in scope on its heap, never the hidden heap.</summary>
    private static string DescribeEffect(Context ctx, Value.VEffect effect)
    {
        if (MutationEffect.HeapOf(effect) is not { } heap) return effect.Family.Name;
        var reference = ctx.Names.FirstOrDefault(n =>
            ctx.Force(n.Value.Type) is Value.VRefTy r && Nbe.Convertible(ctx.Metas, ctx.Width, r.Heap, heap)).Key;
        return reference is null ? effect.Family.Name : $"{effect.Family.Name}({Label(reference)})";
    }

    /// <summary>
    /// E6 through references: a value stored since the match began into a reference
    /// that is not local to it may not carry a function performing an effect the
    /// match handles.
    /// </summary>
    private static void CheckStoredEscape(Context ctx, List<Value> handled, int storedBefore, int since)
    {
        if (handled.Count == 0) return;
        var local = LocalHeaps(ctx, since);
        foreach (var (heap, type) in ctx.Sink.Stored.Skip(storedBefore))
            if (local(heap) is null) CheckEscape(ctx, handled, type);
    }
}
