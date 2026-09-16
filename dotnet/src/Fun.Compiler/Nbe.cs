using System.Collections.Immutable;
using Fun.Kernel;

namespace Fun.Compiler;

/// <summary>
/// Normalisation by evaluation. Evaluation is a loop over a heap-allocated
/// stack of frames, not native recursion per object-level call: a program's
/// call width is bounded by memory, never by the CLR's 1 MB stack, and a
/// captured continuation will be a slice of that stack.
/// </summary>
public static partial class Nbe
{
    // ---- the machine ------------------------------------------------------

    /// <summary>
    /// A continuation frame: what the machine does with the value it is about to
    /// produce. Distinct from <see cref="Frame"/>, an elimination stuck on a neutral.
    /// </summary>
    private abstract partial record Kont
    {
        /// <summary>The callee is evaluated; evaluate the argument next.</summary>
        public sealed record EvalArg(Environment Environment, Term Arg) : Kont;

        /// <summary>The argument is evaluated; apply the callee to it.</summary>
        public sealed record ApplyTo(Value Fn) : Kont;

        /// <summary>The definition is evaluated; push it and run the body.</summary>
        public sealed record LetBody(Environment Environment, Term Body) : Kont;

        /// <summary>The domain is evaluated; close the codomain over the environment.</summary>
        public sealed record PiCodomain(Explicitness Explicitness, Environment Environment, Term Codomain) : Kont;

        /// <summary>One tuple element is evaluated; carry on with the rest.</summary>
        public sealed record ProdItems(Environment Environment, EquatableArray<Term> Rest, EquatableArray<Value> Done, bool IsType)
            : Kont;

        /// <summary>The tuple is evaluated; take its nth element.</summary>
        public sealed record ProjOf(int Index) : Kont;

        /// <summary>The container is evaluated; take its member.</summary>
        public sealed record DotOf(string Name) : Kont;

        /// <summary>The module is evaluated; push its members and run the body.</summary>
        public sealed record OpenBody(Environment Env, EquatableArray<OpenMember> Members, Term Body) : Kont;

        /// <summary>
        /// A slot of a module's binding is evaluated: push it, then carry on with
        /// the binding's remaining slots and the module's remaining bindings.
        /// </summary>
        public sealed record ModuleSlot(
            Environment Env, Slot Slot, EquatableArray<Slot> RestSlots,
            EquatableArray<BindingTerm> RestBindings, EquatableArray<ModuleEntry> Entries) : Kont;

        /// <summary>A module whose bindings are all pushed: build its value.</summary>
        public sealed record ModuleDone(EquatableArray<ModuleEntry> Entries) : Kont;

        /// <summary>A module's open is evaluated: push its members, then carry on.</summary>
        public sealed record ModuleOpen(
            Environment Env, EquatableArray<OpenMember> Members,
            EquatableArray<BindingTerm> RestBindings, EquatableArray<ModuleEntry> Entries) : Kont;
    }

    public static Value Eval(MetaContext mc, Environment env, Term term)
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
                    case Term.InsertedMeta m: value = InsertedMeta(mc, env, m.Id, m.EntryKinds); break;

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

                    case Term.Dot dot:
                        stack.Push(new Kont.DotOf(dot.Name));
                        term = dot.Of;
                        continue;

                    case Term.Open open:
                        stack.Push(new Kont.OpenBody(env, open.Members, open.Body));
                        term = open.Of;
                        continue;

                    case Term.Struct st:
                    {
                        // A finished struct leaves its StructOf frame for the continuation loop.
                        var step = StartStruct(stack, env, st);
                        if (step is { Env: { } e, Term: { } t }) { (env, term) = (e, t); continue; }
                        value = step.Value ?? throw new InvalidOperationException("a step with neither a term nor a value");
                        break;
                    }

                    case Term.RecordConstruct record:
                        (env, term) = StartRecord(stack, env, record) is { Env: { } recordEnv, Term: { } recordTerm } ? (recordEnv, recordTerm) : throw new InvalidOperationException("a record starts with its struct");
                        continue;

                    case Term.Sig sig:
                        value = new Value.VSig(new Closure(env, sig.Body));
                        break;

                    case Term.Module module:
                    {
                        if (module.Signature) stack.Push(new Kont.SignatureOf());
                        // The next piece of work is the first slot of the first
                        // binding with any; a module of none is done at once.
                        if (StartBindings(stack, env, module.Bindings, []) is { } next)
                        {
                            (env, term) = next;
                            continue;
                        }
                        value = FinishModule(stack);
                        break;
                    }

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
                        (env, term) = (f.Environment, f.Arg);
                        goto evaluate;

                    case Kont.ApplyTo f:
                        // Applying a closure continues the loop in its body:
                        // this is where native recursion per call would be.
                        if (f.Fn is Value.VLam lam)
                        {
                            (env, term) = (lam.Body.Environment.Push(value), lam.Body.Body);
                            goto evaluate;
                        }
                        value = ApplyStuck(mc, f.Fn, value);
                        continue;

                    case Kont.LetBody f:
                        (env, term) = (f.Environment.Push(value), f.Body);
                        goto evaluate;

                    case Kont.PiCodomain f:
                        value = new Value.VPi(f.Explicitness, value, new Closure(f.Environment, f.Codomain));
                        continue;

                    case Kont.ProjOf f:
                        value = Project(value, f.Index);
                        continue;

                    case Kont.DotOf f:
                        value = DotValue(value, f.Name);
                        continue;

                    case Kont.StructOf f:
                        value = AsStruct(f, value);
                        continue;

                    case Kont.SignatureOf:
                        value = AsSignature(value);
                        continue;

                    case Kont.StructField f:
                    {
                        var step = ResumeStructField(stack, f, value);
                        if (step is { Env: { } e, Term: { } t }) { (env, term) = (e, t); goto evaluate; }
                        value = step.Value ?? throw new InvalidOperationException("a step with neither a term nor a value");
                        continue;
                    }

                    case Kont.RecordType f:
                    {
                        var step = ResumeRecord(stack, f, value);
                        if (step is { Env: { } e, Term: { } t }) { (env, term) = (e, t); goto evaluate; }
                        value = step.Value ?? throw new InvalidOperationException("a step with neither a term nor a value");
                        continue;
                    }

                    case Kont.RecordField f:
                    {
                        var step = ResumeRecordField(stack, f, value);
                        if (step is { Env: { } e, Term: { } t }) { (env, term) = (e, t); goto evaluate; }
                        value = step.Value ?? throw new InvalidOperationException("a step with neither a term nor a value");
                        continue;
                    }

                    case Kont.OpenBody f:
                        (env, term) = (PushOpenMembers(f.Env, value, f.Members), f.Body);
                        goto evaluate;

                    case Kont.ModuleSlot f:
                    {
                        var pushed = f.Env.Push(value);
                        var entries = f.Slot.Name is { } name
                            ? f.Entries.Add(new ModuleEntry.Field(name, f.Slot.Kind, value))
                            : f.Entries;
                        if (!f.RestSlots.IsEmpty)
                        {
                            stack.Push(f with { Env = pushed, Slot = f.RestSlots[0], RestSlots = f.RestSlots.RemoveAt(0), Entries = entries });
                            (env, term) = (pushed, Def(f.RestSlots[0]));
                            goto evaluate;
                        }
                        if (StartBindings(stack, pushed, f.RestBindings, entries) is { } next)
                        {
                            (env, term) = next;
                            goto evaluate;
                        }
                        value = FinishModule(stack);
                        continue;
                    }

                    case Kont.ModuleOpen f:
                    {
                        var pushed = PushOpenMembers(f.Env, value, f.Members);
                        if (StartBindings(stack, pushed, f.RestBindings, f.Entries) is { } next)
                        {
                            (env, term) = next;
                            goto evaluate;
                        }
                        value = FinishModule(stack);
                        continue;
                    }


                    case Kont.ProdItems f:
                    {
                        var done = f.Done.Add(value);
                        if (f.Rest.IsEmpty)
                        {
                            value = f.IsType ? new Value.VProdTy(done) : new Value.VProd(done);
                            continue;
                        }
                        stack.Push(f with { Rest = f.Rest.RemoveAt(0), Done = done });
                        (env, term) = (f.Environment, f.Rest[0]);
                        goto evaluate;
                    }
                }
            }

        evaluate: ;
        }
    }

    /// <summary>
    /// Pushes the frame for a module's next binding and returns the term to
    /// evaluate for it, or pushes the finished module and returns null. A binding
    /// pushes exactly its slots (I2); an open pushes its members.
    /// </summary>
    private static (Environment, Term)? StartBindings(
        Stack<Kont> stack, Environment env, EquatableArray<BindingTerm> bindings, EquatableArray<ModuleEntry> entries)
    {
        if (bindings.IsEmpty)
        {
            stack.Push(new Kont.ModuleDone(entries));
            return null;
        }
        var binding = bindings[0];
        var rest = bindings.RemoveAt(0);
        switch (binding)
        {
            case BindingTerm.Open open:
                stack.Push(new Kont.ModuleOpen(env, open.Members, rest, entries));
                return (env, open.Of);
            default:
                var slots = binding.Slots() ?? throw new InvalidOperationException("a binding with no slot list");
                if (slots.IsEmpty) return StartBindings(stack, env, rest, entries);
                stack.Push(new Kont.ModuleSlot(env, slots[0], slots.RemoveAt(0), rest, entries));
                return (env, Def(slots[0]));
        }
    }

    /// <summary>Pops the finished module <see cref="StartBindings"/> left on the stack.</summary>
    private static Value FinishModule(Stack<Kont> stack) =>
        stack.Pop() is Kont.ModuleDone done
            ? new Value.VModule(done.Entries, Partial: false)
            : throw new InvalidOperationException("a module finished without its frame");

    private static Term Def(Slot slot) => slot.Source switch
    {
        SlotSource.Def d => d.Term,
        _ => throw new InvalidOperationException($"unhandled slot source {slot.Source.GetType().Name}"),
    };

    /// <summary>
    /// A container's member by label: the last entry of that name (I3). Stuck on
    /// a neutral container.
    /// </summary>
    public static Value DotValue(Value of, string name) => of switch
    {
        Value.VModule m => VisibleMember(m.Entries, name) ?? throw new FunException($"no member `{name}`"),
        Value.VStruct st => VisibleMember(st.Entries, name) ?? throw new FunException($"no member `{name}`"),
        Value.VRecord r => r.Fields.FirstOrDefault(f => f.Name == name) is { Value: { } field } ? field : throw new FunException($"no field `{name}`"),
        Value.VNeutral n => n with { Ty = Value.VU.Instance, Frames = n.Frames.Add(new Frame.FDot(name)) },
        Value.VMeta f => new Value.VNeutral(Value.VU.Instance, new Head.HMeta(f.Id), Spine(f.Spine).Add(new Frame.FDot(name))),
        Value.VVar r => new Value.VNeutral(Value.VU.Instance, new Head.HVar(r.Level), Spine(r.Spine).Add(new Frame.FDot(name))),
        _ => throw new FunException($"member access `.{name}` on a non-module"),
    };

    /// <summary>Pushes each opened member, in order, projected from the module.</summary>
    public static Environment PushOpenMembers(Environment env, Value module, EquatableArray<OpenMember> members) =>
        members.Aggregate(env, (acc, member) => member switch
        {
            OpenMember.Field f => acc.Push(DotValue(module, f.Name)),
            _ => throw new InvalidOperationException($"unhandled open member {member.GetType().Name}"),
        });

    /// <summary>Applies <paramref name="fn"/> to <paramref name="arg"/>.</summary>
    public static Value Apply(MetaContext mc, Value fn, Value arg) =>
        fn is Value.VLam lam
            ? Eval(mc, lam.Body.Environment.Push(arg), lam.Body.Body)
            : ApplyStuck(mc, fn, arg);

    /// <summary>Application to something that is not a closure: the result is stuck.</summary>
    private static Value ApplyStuck(MetaContext mc, Value fn, Value arg) => Force(mc, fn) switch
    {
        Value.VLam lam => Eval(mc, lam.Body.Environment.Push(arg), lam.Body.Body),
        Value.VNeutral n => n with { Ty = ApplyTy(mc, n.Ty, arg), Frames = n.Frames.Add(new Frame.FApp(arg)) },
        Value.VMeta f => f with { Spine = f.Spine.Add(arg) },
        Value.VVar r => r with { Spine = r.Spine.Add(arg) },
        var other => throw new FunException($"applying non-function: {other.GetType().Name}"),
    };

    private static Value ApplyTy(MetaContext mc, Value ty, Value arg) =>
        ty is Value.VPi pi ? ApplyClosure(mc, pi.Codomain, arg) : Value.VU.Instance;

    public static Value ApplyClosure(MetaContext mc, Closure closure, Value arg) =>
        Eval(mc, closure.Environment.Push(arg), closure.Body);

    private static Value Project(Value of, int index) => of switch
    {
        Value.VProd p => p.Items[index],
        Value.VProdTy p => p.Items[index],
        Value.VNeutral n => n with { Ty = Value.VU.Instance, Frames = n.Frames.Add(new Frame.FProj(index)) },
        Value.VMeta f => new Value.VNeutral(Value.VU.Instance, new Head.HMeta(f.Id), Spine(f.Spine).Add(new Frame.FProj(index))),
        Value.VVar r => new Value.VNeutral(Value.VU.Instance, new Head.HVar(r.Level), Spine(r.Spine).Add(new Frame.FProj(index))),
        _ => throw new FunException("projection of a non-tuple"),
    };

    private static EquatableArray<Frame> Spine(EquatableArray<Value> spine) =>
        [.. spine.Select(v => (Frame)new Frame.FApp(v))];

    // ---- metas ----------------------------------------------------

    private static Value Meta(MetaContext mc, int id) =>
        mc.Solution(id) ?? new Value.VMeta(id, []);

    /// <summary>
    /// A meta as the elaborator created it: applied to every entry in
    /// the context that is a bound entry, skipping the defined ones,
    /// whose values are already known.
    /// </summary>
    private static Value InsertedMeta(MetaContext mc, Environment env, int id, EquatableArray<EntryKind> kinds)
    {
        if (kinds.Length != env.Count)
            throw new InvalidOperationException(
                $"bd mask length mismatch: {kinds.Length} entries against an environment of {env.Count}");
        var value = Meta(mc, id);
        for (var i = kinds.Length - 1; i >= 0; i--)
            if (kinds[i] == EntryKind.Bound)
                value = ApplyStuck(mc, value, env[i]);
        return value;
    }

    // ---- readback ---------------------------------------------------------

    /// <summary>
    /// Reads a value back as a term at <paramref name="width"/> entries, turning
    /// levels back into indices.
    /// </summary>
    // Recurses natively: its width is the value's structure (a type), never a
    // program's call width.
    public static Term Quote(MetaContext mc, int width, Value value)
    {
        var fresh = new Value.VVar(width, []);
        return Force(mc, value) switch
        {
            Value.VLam lam => new Term.Lam(Quote(mc, width + 1, ApplyClosure(mc, lam.Body, fresh))),
            Value.VPi pi => new Term.Pi(pi.Explicitness, Quote(mc, width, pi.Domain),
                Quote(mc, width + 1, ApplyClosure(mc, pi.Codomain, fresh))),
            Value.VU => Term.U.Instance,
            Value.VAtom a => new Term.Atom(a.Atom),
            Value.VAtomTy a => new Term.AtomTy(a.Ty),
            Value.VProd p => new Term.Prod([.. p.Items.Select(i => Quote(mc, width, i))]),
            Value.VProdTy p => new Term.ProdTy([.. p.Items.Select(i => Quote(mc, width, i))]),
            Value.VMeta f => QuoteSpine(mc, width, new Term.Meta(f.Id), f.Spine),
            Value.VVar r => QuoteSpine(mc, width, new Term.Var(LevelToIndex(width, r.Level)), r.Spine),
            // Evaluating the module pushes one entry per binding, so the ith
            // binding's term is read i entries further in.
            Value.VModule m => new Term.Module([.. m.Entries.Select((e, i) => e switch
            {
                ModuleEntry.Field f => (BindingTerm)new BindingTerm.Let(f.Name, f.Kind, Quote(mc, width + i, f.Value)),
                _ => throw new InvalidOperationException($"unhandled module entry {e.GetType().Name}"),
            })], m.Partial),
            Value.VStruct st => QuoteStruct(mc, width, st),
            Value.VRecord r => new Term.RecordConstruct(Quote(mc, width, r.Type), [.. r.Fields.Select(f => (f.Name, Quote(mc, width, f.Value)))]),
            Value.VSig sig => new Term.Sig(Quote(mc, width + 1, ApplyClosure(mc, sig.Body, fresh))),
            Value.VNeutral n => n.Frames.Aggregate(QuoteHead(width, n.Head), (acc, frame) => frame switch
            {
                Frame.FApp a => new Term.Ap(acc, Explicitness.Explicit, Quote(mc, width, a.Arg)),
                Frame.FProj p => new Term.Proj(acc, p.Index),
                Frame.FDot d => new Term.Dot(acc, d.Name),
                _ => throw new InvalidOperationException($"unhandled frame {frame.GetType().Name}"),
            }),
            var other => throw new NotImplementedException($"not ported yet: reading back {other.GetType().Name}"),
        };
    }

    private static Term QuoteSpine(MetaContext mc, int width, Term head, EquatableArray<Value> spine) =>
        spine.Aggregate(head, (acc, v) => new Term.Ap(acc, Explicitness.Explicit, Quote(mc, width, v)));

    private static Term QuoteHead(int width, Head head) => head switch
    {
        Head.HVar v => new Term.Var(LevelToIndex(width, v.Level)),
        Head.HMeta m => new Term.Meta(m.Id),
        Head.HPrim p => new Term.Prim(p.Name),
        _ => throw new InvalidOperationException($"unhandled head {head.GetType().Name}"),
    };

    /// <summary>A level as the index it is at <paramref name="width"/> entries.</summary>
    public static int LevelToIndex(int width, int level) => width - level - 1;

    /// <summary>A value with any solved meta at its head resolved away.</summary>
    public static Value Force(MetaContext mc, Value value)
    {
        while (value is Value.VMeta meta && mc.Solution(meta.Id) is { } solution)
            value = meta.Spine.Aggregate(solution, (f, a) => ApplyStuck(mc, f, a));
        return value;
    }
}
