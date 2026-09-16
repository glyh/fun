using System.Collections.Immutable;
using Fun.Kernel;

namespace Fun.Compiler;

/// <summary>
/// Normalisation by evaluation. Evaluation is a loop over a heap-allocated
/// stack of frames, not native recursion per object-level call: a program's
/// call depth is bounded by memory, never by the CLR's 1 MB stack, and a
/// captured continuation will be a slice of that stack.
/// </summary>
public static class Nbe
{
    // ---- the machine ------------------------------------------------------

    /// <summary>
    /// A continuation frame: what the machine does with the value it is about to
    /// produce. Distinct from <see cref="Frame"/>, an elimination stuck on a neutral.
    /// </summary>
    private abstract record Kont
    {
        /// <summary>The callee is evaluated; evaluate the argument next.</summary>
        public sealed record EvalArg(Env Env, Term Arg) : Kont;

        /// <summary>The argument is evaluated; apply the callee to it.</summary>
        public sealed record ApplyTo(Value Fn) : Kont;

        /// <summary>The definition is evaluated; push it and run the body.</summary>
        public sealed record LetBody(Env Env, Term Body) : Kont;

        /// <summary>The domain is evaluated; close the codomain over the scope.</summary>
        public sealed record PiCodomain(Explicitness Explicitness, Env Env, Term Codomain) : Kont;

        /// <summary>One tuple element is evaluated; carry on with the rest.</summary>
        public sealed record ProdItems(Env Env, ImmutableArray<Term> Rest, ImmutableArray<Value> Done, bool IsType)
            : Kont;

        /// <summary>The tuple is evaluated; take its nth element.</summary>
        public sealed record ProjOf(int Index) : Kont;
    }

    public static Value Eval(MetaContext mc, Env env, Term term)
    {
        // The frames the machine still owes work to, innermost last.
        var stack = new Stack<Kont>();

        while (true)
        {
            // Reduce the term to a value, pushing a frame for anything that
            // needs a sub-evaluation first.
            Value value;
            while (true)
            {
                switch (term)
                {
                    case Term.Var v: value = env[v.Index]; break;
                    case Term.Lam l: value = new Value.VLam(new Closure(env, l.Body)); break;
                    case Term.U: value = Value.VU.Instance; break;
                    case Term.Atom a: value = new Value.VAtom(a.Value); break;
                    case Term.AtomTy a: value = new Value.VAtomTy(a.Ty); break;
                    case Term.Prim p:
                        value = new Value.VNeutral(Value.VU.Instance, new Head.HPrim(p.Name), []);
                        break;
                    case Term.Meta m: value = Meta(mc, m.Id); break;
                    case Term.InsertedMeta m: value = InsertedMeta(mc, env, m.Id, m.Bds); break;

                    case Term.Ap ap:
                        stack.Push(new Kont.EvalArg(env, ap.Arg));
                        term = ap.Fn;
                        continue;

                    case Term.Let let:
                        stack.Push(new Kont.LetBody(env, let.Body));
                        term = let.Def;
                        continue;

                    case Term.Pi pi:
                        stack.Push(new Kont.PiCodomain(pi.Explicitness, env, pi.Codomain));
                        term = pi.Domain;
                        continue;

                    case Term.Proj proj:
                        stack.Push(new Kont.ProjOf(proj.Index));
                        term = proj.Of;
                        continue;

                    case Term.Prod { Items.IsEmpty: true }: value = new Value.VProd([]); break;
                    case Term.ProdTy { Items.IsEmpty: true }: value = new Value.VProdTy([]); break;

                    case Term.Prod prod:
                        stack.Push(new Kont.ProdItems(env, prod.Items.RemoveAt(0), [], IsType: false));
                        term = prod.Items[0];
                        continue;

                    case Term.ProdTy prod:
                        stack.Push(new Kont.ProdItems(env, prod.Items.RemoveAt(0), [], IsType: true));
                        term = prod.Items[0];
                        continue;

                    default:
                        throw new NotImplementedException($"not ported yet: evaluating {term.GetType().Name}");
                }
                break;
            }

            // Hand the value to the frame waiting for it. A frame that resumes
            // a term sets `term` and goes round again rather than recursing.
            while (true)
            {
                if (stack.Count == 0) return value;
                switch (stack.Pop())
                {
                    case Kont.EvalArg f:
                        stack.Push(new Kont.ApplyTo(value));
                        (env, term) = (f.Env, f.Arg);
                        goto evaluate;

                    case Kont.ApplyTo f:
                        // Applying a closure continues the loop in its body:
                        // this is where native recursion per call would be.
                        if (f.Fn is Value.VLam lam)
                        {
                            (env, term) = (lam.Body.Env.Push(value), lam.Body.Body);
                            goto evaluate;
                        }
                        value = ApplyStuck(mc, f.Fn, value);
                        continue;

                    case Kont.LetBody f:
                        (env, term) = (f.Env.Push(value), f.Body);
                        goto evaluate;

                    case Kont.PiCodomain f:
                        value = new Value.VPi(f.Explicitness, value, new Closure(f.Env, f.Codomain));
                        continue;

                    case Kont.ProjOf f:
                        value = Project(value, f.Index);
                        continue;

                    case Kont.ProdItems f:
                    {
                        var done = f.Done.Add(value);
                        if (f.Rest.IsEmpty)
                        {
                            value = f.IsType ? new Value.VProdTy(done) : new Value.VProd(done);
                            continue;
                        }
                        stack.Push(f with { Rest = f.Rest.RemoveAt(0), Done = done });
                        (env, term) = (f.Env, f.Rest[0]);
                        goto evaluate;
                    }
                }
            }

        evaluate: ;
        }
    }

    /// <summary>Applies <paramref name="fn"/> to <paramref name="arg"/>.</summary>
    public static Value Apply(MetaContext mc, Value fn, Value arg) =>
        fn is Value.VLam lam
            ? Eval(mc, lam.Body.Env.Push(arg), lam.Body.Body)
            : ApplyStuck(mc, fn, arg);

    /// <summary>Application to something that is not a closure: the result is stuck.</summary>
    private static Value ApplyStuck(MetaContext mc, Value fn, Value arg) => Force(mc, fn) switch
    {
        Value.VLam lam => Eval(mc, lam.Body.Env.Push(arg), lam.Body.Body),
        Value.VNeutral n => n with { Ty = ApplyTy(mc, n.Ty, arg), Frames = n.Frames.Add(new Frame.FApp(arg)) },
        Value.VFlex f => f with { Spine = f.Spine.Add(arg) },
        Value.VRigid r => r with { Spine = r.Spine.Add(arg) },
        var other => throw new FunException($"applying non-function: {other.GetType().Name}"),
    };

    private static Value ApplyTy(MetaContext mc, Value ty, Value arg) =>
        ty is Value.VPi pi ? ApplyClosure(mc, pi.Codomain, arg) : Value.VU.Instance;

    public static Value ApplyClosure(MetaContext mc, Closure closure, Value arg) =>
        Eval(mc, closure.Env.Push(arg), closure.Body);

    private static Value Project(Value of, int index) => of switch
    {
        Value.VProd p => p.Items[index],
        Value.VProdTy p => p.Items[index],
        Value.VNeutral n => n with { Ty = Value.VU.Instance, Frames = n.Frames.Add(new Frame.FProj(index)) },
        Value.VFlex f => new Value.VNeutral(Value.VU.Instance, new Head.HMeta(f.Id), Spine(f.Spine).Add(new Frame.FProj(index))),
        Value.VRigid r => new Value.VNeutral(Value.VU.Instance, new Head.HVar(r.Level), Spine(r.Spine).Add(new Frame.FProj(index))),
        _ => throw new FunException("projection of a non-tuple"),
    };

    private static ImmutableArray<Frame> Spine(ImmutableArray<Value> spine) =>
        [.. spine.Select(v => (Frame)new Frame.FApp(v))];

    // ---- metavariables ----------------------------------------------------

    private static Value Meta(MetaContext mc, int id) =>
        mc.Solution(id) ?? new Value.VFlex(id, []);

    /// <summary>
    /// A metavariable as the elaborator created it: applied to every entry in
    /// scope that a binder introduced, skipping the ones a definition did,
    /// whose values are already known.
    /// </summary>
    private static Value InsertedMeta(MetaContext mc, Env env, int id, ImmutableArray<Bd> bds)
    {
        if (bds.Length != env.Count)
            throw new InvalidOperationException(
                $"bd mask length mismatch: {bds.Length} entries against an environment of {env.Count}");
        var value = Meta(mc, id);
        for (var i = bds.Length - 1; i >= 0; i--)
            if (bds[i] == Bd.Bound)
                value = ApplyStuck(mc, value, env[i]);
        return value;
    }

    // ---- readback ---------------------------------------------------------

    /// <summary>
    /// Reads a value back as a term at <paramref name="depth"/> entries, turning
    /// levels back into indices.
    /// </summary>
    // Recurses natively: its depth is the value's structure (a type), never a
    // program's call depth.
    public static Term Quote(MetaContext mc, int depth, Value value)
    {
        var fresh = new Value.VRigid(depth, []);
        return Force(mc, value) switch
        {
            Value.VLam lam => new Term.Lam(Quote(mc, depth + 1, ApplyClosure(mc, lam.Body, fresh))),
            Value.VPi pi => new Term.Pi(pi.Explicitness, Quote(mc, depth, pi.Domain),
                Quote(mc, depth + 1, ApplyClosure(mc, pi.Codomain, fresh))),
            Value.VU => Term.U.Instance,
            Value.VAtom a => new Term.Atom(a.Atom),
            Value.VAtomTy a => new Term.AtomTy(a.Ty),
            Value.VProd p => new Term.Prod([.. p.Items.Select(i => Quote(mc, depth, i))]),
            Value.VProdTy p => new Term.ProdTy([.. p.Items.Select(i => Quote(mc, depth, i))]),
            Value.VFlex f => QuoteSpine(mc, depth, new Term.Meta(f.Id), f.Spine),
            Value.VRigid r => QuoteSpine(mc, depth, new Term.Var(LevelToIndex(depth, r.Level)), r.Spine),
            Value.VNeutral n => n.Frames.Aggregate(QuoteHead(depth, n.Head), (acc, frame) => frame switch
            {
                Frame.FApp a => new Term.Ap(acc, Explicitness.Explicit, Quote(mc, depth, a.Arg)),
                Frame.FProj p => new Term.Proj(acc, p.Index),
                _ => throw new InvalidOperationException($"unhandled frame {frame.GetType().Name}"),
            }),
            var other => throw new NotImplementedException($"not ported yet: reading back {other.GetType().Name}"),
        };
    }

    private static Term QuoteSpine(MetaContext mc, int depth, Term head, ImmutableArray<Value> spine) =>
        spine.Aggregate(head, (acc, v) => new Term.Ap(acc, Explicitness.Explicit, Quote(mc, depth, v)));

    private static Term QuoteHead(int depth, Head head) => head switch
    {
        Head.HVar v => new Term.Var(LevelToIndex(depth, v.Level)),
        Head.HMeta m => new Term.Meta(m.Id),
        Head.HPrim p => new Term.Prim(p.Name),
        _ => throw new InvalidOperationException($"unhandled head {head.GetType().Name}"),
    };

    /// <summary>A level as the index it is at <paramref name="depth"/> entries.</summary>
    public static int LevelToIndex(int depth, int level) => depth - level - 1;

    /// <summary>A value with any solved metavariable at its head resolved away.</summary>
    public static Value Force(MetaContext mc, Value value)
    {
        while (value is Value.VFlex flex && mc.Solution(flex.Id) is { } solution)
            value = flex.Spine.Aggregate(solution, (f, a) => ApplyStuck(mc, f, a));
        return value;
    }
}
