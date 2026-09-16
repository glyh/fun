using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Nbe
{
    private abstract partial record Kont
    {
        /// <summary>A performed effect instance is evaluated; evaluate the argument next.</summary>
        public sealed record PerformOn(Environment Env, string Op, Term Arg) : Kont;

        /// <summary>The argument is evaluated: raise the request.</summary>
        public sealed record PerformArg(Value Instance, string Op) : Kont;

        /// <summary>
        /// A handler. Below its scrutinee (<paramref name="InBody"/> false) a value
        /// chooses an arm; below an arm's body it passes through. Either way a request
        /// raised above it may be handled here (deep, E8). <paramref name="Instances"/>
        /// are its effect branches' instances, evaluated where the match began.
        /// </summary>
        public sealed record Handle(Environment Env, Term.Match Match, EquatableArray<Value> Instances, bool InBody) : Kont;

        /// <summary>
        /// A call with an open row (E5): a request passing it whose instance is none
        /// of <paramref name="Named"/> skips the handlers it names.
        /// </summary>
        public sealed record TunnelFrame(Environment Env, EquatableArray<Term> Named, EquatableArray<int> Handlers) : Kont;
    }

    /// <summary>A continuation: the frames a handler captured, bottom first, the handler's own frame first of all.</summary>
    private sealed class CapturedFrames(Kont[] frames) : Continuation
    {
        public Kont[] Frames => frames;
    }

    /// <summary>The first step of an effect form, or null when the term is not one.</summary>
    private static (Environment Env, Term Term)? StartEffects(MetaContext mc, Stack<Kont> stack, Environment env, Term term, out Value? value)
    {
        value = null;
        switch (term)
        {
            case Term.EffectRowTy:
                value = Value.VEffectRowTy.Instance;
                return null;
            case Term.EffectRowLit lit:
                value = EvalRow(mc, env, lit.Row);
                return null;
            case Term.Effect e:
                value = new Value.VEffect(e.Family, e.Environment, [.. e.Params.Select(p => Eval(mc, env, p))]);
                return null;
            case Term.EffectDecl decl:
                value = new Value.VEffect(decl.Family, env, []);
                return null;
            case Term.Perform perform:
                stack.Push(new Kont.PerformOn(env, perform.Op, perform.Arg));
                return (env, perform.Instance);
            case Term.Tunnel tunnel:
                stack.Push(new Kont.TunnelFrame(env, tunnel.Named, tunnel.Handlers));
                return (env, tunnel.Body);
            case Term.Match { EffectBranches.IsEmpty: false } match:
                stack.Push(new Kont.Handle(env, match, [.. match.EffectBranches.Select(b => Eval(mc, env, b.Instance))], InBody: false));
                return (env, match.Scrutinee);
            default:
                throw new InvalidOperationException($"not an effect form: {term.GetType().Name}");
        }
    }

    /// <summary>
    /// Raises a request: pops frames until a handler that is not skipped has a
    /// branch for it. The popped frames, that handler's included, are the
    /// continuation; the branch body then runs under the same handler.
    /// </summary>
    private static (Environment, Term) Raise(MetaContext mc, Stack<Kont> stack, Value instance, string op, Value arg)
    {
        var popped = new List<Kont>();
        var skips = new HashSet<int>();
        while (stack.Count > 0)
        {
            var frame = stack.Pop();
            popped.Add(frame);
            switch (frame)
            {
                case Kont.TunnelFrame tunnel:
                    if (!tunnel.Named.Any(named => SameEffect(mc, Eval(mc, tunnel.Env, named), instance)))
                        skips.UnionWith(tunnel.Handlers);
                    break;

                case Kont.Handle handler when !skips.Contains(handler.Match.Handler):
                    for (var i = 0; i < handler.Instances.Length; i++)
                    {
                        var branch = handler.Match.EffectBranches[i];
                        if (branch.Op != op || !SameEffect(mc, handler.Instances[i], instance)) continue;
                        popped.Reverse();
                        var continuation = new Value.VCont(new CapturedFrames([.. popped]));
                        stack.Push(handler with { InBody = true });
                        // The argument pattern is exhaustive (checked at elaboration), so its tree always selects.
                        var (armEnv, body) = SelectArm(mc, handler.Env, arg, new Term.Match(Term.U.Instance, [branch.Body], branch.Argument));
                        return (armEnv.Push(continuation), body);
                    }
                    break;
            }
        }
        // The checker only evaluates terms that perform nothing; reaching here while
        // checking means a check-time evaluation site was not ported with that guard.
        if (mc.Budget.Checking)
            throw new NotImplementedException($"not ported yet: the checker evaluated a term that performs {Describe(instance)}.{op}");
        throw new FunException($"unhandled effect: {Describe(instance)}.{op}");
    }

    /// <summary>
    /// Resumes a continuation with <paramref name="arg"/>: its frames go back on
    /// the stack, so the value flows into them - and out through the handler -
    /// before reaching whatever called <c>resume</c>.
    /// </summary>
    private static Value Resume(Stack<Kont> stack, Value.VCont cont, Value arg)
    {
        if (cont.Continuation.Used) throw new FunException("continuation already used");
        cont.Continuation.Used = true;
        foreach (var frame in ((CapturedFrames)cont.Continuation).Frames) stack.Push(frame);
        return arg;
    }

    private static string Describe(Value instance) => instance is Value.VEffect e ? e.Family.Name : instance.GetType().Name;

    // A width no context reaches, so comparing by readback never mistakes a real
    // variable for a binder introduced while reading back.
    private const int ComparisonWidth = 1 << 20;

    /// <summary>
    /// Whether two effect instances are the same (E1): the same family, and
    /// parameters that read back alike.
    /// </summary>
    public static bool SameEffect(MetaContext mc, Value left, Value right) =>
        Force(mc, left) is Value.VEffect a && Force(mc, right) is Value.VEffect b
        && a.Family.Id == b.Family.Id && a.Params.Length == b.Params.Length
        && a.Params.Zip(b.Params).All(p => Convertible(mc, ComparisonWidth, p.First, p.Second));

    // ---- rows -------------------------------------------------------------

    public static Value.VEffectRow EvalRow(MetaContext mc, Environment env, RowTerm row) =>
        row.IsPure
            ? Value.VEffectRow.Pure
            : NormalizeRow(mc, [.. row.Effects.Select(e => Eval(mc, env, e))], [.. row.Tails.Select(t => Eval(mc, env, t))]);

    /// <summary>An arrow's row, read under the arrow's binder at <paramref name="binder"/>.</summary>
    public static Value.VEffectRow EvalRowClosure(MetaContext mc, RowClosure row, Value binder) =>
        row.Row.IsPure ? Value.VEffectRow.Pure : EvalRow(mc, row.Environment.Push(binder), row.Row);

    /// <summary>
    /// A row in normal form: a tail solved to a row is spliced in - its effects join
    /// the known ones, its tails join the tails - so a union of tails disappears as
    /// its variables are solved (E2). Effects and tails are each kept once.
    /// </summary>
    public static Value.VEffectRow NormalizeRow(MetaContext mc, EquatableArray<Value> effects, EquatableArray<Value> tails)
    {
        var knownEffects = new List<Value>();
        var openTails = new List<Value>();
        void AddEffect(Value e)
        {
            if (!knownEffects.Any(k => Convertible(mc, ComparisonWidth, k, e))) knownEffects.Add(e);
        }
        void AddTail(Value tail)
        {
            switch (Force(mc, tail))
            {
                case Value.VEffectRow row:
                    foreach (var e in row.Effects) AddEffect(e);
                    foreach (var t in row.Tails) AddTail(t);
                    break;
                case var open:
                    if (!openTails.Any(k => Convertible(mc, ComparisonWidth, k, open))) openTails.Add(open);
                    break;
            }
        }
        foreach (var e in effects) AddEffect(e);
        foreach (var t in tails) AddTail(t);
        return new Value.VEffectRow([.. knownEffects], [.. openTails]);
    }

    private static RowTerm QuoteRow(MetaContext mc, int width, Value.VEffectRow row) =>
        new([.. row.Effects.Select(e => Quote(mc, width, e))], [.. row.Tails.Select(t => Quote(mc, width, t))]);

    /// <summary>An arrow's row read back under its binder, a level further in.</summary>
    private static RowTerm QuoteRowClosure(MetaContext mc, int width, RowClosure row) =>
        row.Row.IsPure ? RowTerm.Pure : QuoteRow(mc, width + 1, EvalRowClosure(mc, row, new Value.VVar(width, [])));

    private static Term QuoteEffect(MetaContext mc, int width, Value.VEffect effect) =>
        new Term.Effect(effect.Family, effect.Environment, [.. effect.Params.Select(p => Quote(mc, width, p))]);

    /// <summary>An operation's input and output types at an instance: read under its parameters, in order.</summary>
    public static (Value Input, Value Output) OperationTypes(MetaContext mc, Value.VEffect instance, EffectOperation operation)
    {
        var env = instance.Params.Aggregate(instance.Environment, (e, p) => e.Push(p));
        return (Eval(mc, env, operation.Input), Eval(mc, env, operation.Output));
    }
}
