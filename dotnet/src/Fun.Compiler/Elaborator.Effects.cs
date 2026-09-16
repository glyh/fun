using System.Collections.Immutable;
using Fun.Kernel;

namespace Fun.Compiler;

/// <summary>
/// What the form being elaborated performs: the effects, and the row variables
/// whose effects it also has. Infer and check emit into it as they go; a fresh one
/// starts where effects stop flowing outward - a lambda's body, a type, a handled
/// scrutinee, a unit.
/// </summary>
public sealed class EffectSink
{
    public List<Value> Effects { get; } = [];
    public List<Value> Tails { get; } = [];
    public bool IsEmpty => Effects.Count == 0 && Tails.Count == 0;
}

public sealed partial record Context
{
    public EffectSink Sink { get; init; } = new();

    /// <summary>
    /// The handlers lexically enclosing this point within the current function
    /// body, innermost last. A call whose row has an open tail tunnels past these (E5).
    /// </summary>
    public ImmutableList<int> HandlerScopes { get; init; } = [];

    /// <summary>Inside an effect branch: the continuation's entry, which <c>resume</c> applies (E9).</summary>
    public Entry? ResumeEntry { get; init; }
}

public static partial class Elaborator
{
    private static int _nextEffectFamily;
    private static int _nextHandler;

    // ---- sinks ------------------------------------------------------------

    /// <summary>The form being elaborated performs <paramref name="effects"/> and has the row variables <paramref name="tails"/>.</summary>
    private static void Emit(Context ctx, IEnumerable<Value> effects, IEnumerable<Value> tails)
    {
        foreach (var e in effects)
            if (!ctx.Sink.Effects.Any(k => Nbe.Convertible(ctx.Metas, ctx.Width, k, e))) ctx.Sink.Effects.Add(e);
        foreach (var t in tails)
            if (!ctx.Sink.Tails.Any(k => Nbe.Convertible(ctx.Metas, ctx.Width, k, t))) ctx.Sink.Tails.Add(t);
    }

    private static void Emit(Context ctx, EffectSink performed) => Emit(ctx, performed.Effects, performed.Tails);

    /// <summary><paramref name="elaborate"/> in a fresh sink: its result, and what it performed, which does not reach the enclosing form.</summary>
    private static (T, EffectSink) Collecting<T>(Context ctx, Func<Context, T> elaborate)
    {
        var sink = new EffectSink();
        return (elaborate(ctx with { Sink = sink }), sink);
    }

    /// <summary>A form that must perform nothing - a type is evaluated at check time (E4).</summary>
    private static T Pure<T>(Context ctx, Func<Context, T> elaborate)
    {
        var (result, performed) = Collecting(ctx, elaborate);
        RequireEmpty(ctx, performed);
        return result;
    }

    private static void RequireEmpty(Context ctx, EffectSink performed)
    {
        if (performed.Effects.Count > 0) throw Unhandled(ctx, performed.Effects, inFunction: false);
        foreach (var tail in performed.Tails) ctx.Unify(tail, Value.VEffectRow.Pure);
    }

    private static FunException Unhandled(Context ctx, IEnumerable<Value> effects, bool inFunction)
    {
        var names = string.Join(", ", effects.Select(e => ctx.Force(e) is Value.VEffect v ? v.Family.Name : e.GetType().Name));
        return new FunException(inFunction
            ? $"effects in a pure result: {names}; write ->{{E}} T or ~> T"
            : $"unhandled effects: {names}");
    }

    /// <summary>
    /// A program's entry, or a unit's: it performs nothing the runtime handles, and
    /// every row written <c>_</c> since <paramref name="since"/> is solved.
    /// </summary>
    // ponytail: the runtime's handler discharges heap effects, which arrive with refs.
    public static void RequireHandledAtEntry(Context ctx, EffectSink performed, int since)
    {
        RequireEmpty(ctx, performed);
        foreach (var id in ctx.Metas.WrittenRows)
            if (id >= since && ctx.Metas.Solution(id) is null)
                throw new FunException("an effect row written _ is never solved: write the row");
    }

    /// <summary>
    /// A body's effects against the row its function declares. Effects the row names
    /// are covered; what is left goes to the row's single tail, or is an error.
    /// </summary>
    private static void CheckEffectSubset(Context ctx, EffectSink actual, Value.VEffectRow expected, bool inFunction)
    {
        var unmatched = actual.Effects
            .Where(e => !expected.Effects.Any(x => Unify.TryValues(ctx.Metas, ctx.Width, e, x)))
            .ToList();
        var tails = actual.Tails
            .Where(t => !expected.Tails.Any(x => Nbe.Convertible(ctx.Metas, ctx.Width, t, x)))
            .ToList();

        bool Written(Value tail) => ctx.Force(tail) is Value.VMeta m && ctx.Metas.WrittenRows.Contains(m.Id);

        switch (unmatched.Count, tails.Count, expected.Tails.Length)
        {
            // A written row checked against a body that performs nothing at all is empty.
            case (0, 0, _) when actual.Tails.Count == 0:
                foreach (var tail in expected.Tails.Where(Written)) ctx.Unify(tail, Value.VEffectRow.Pure);
                return;
            case (0, 0, _):
                return;
            case (_, _, 1):
                ctx.Unify(expected.Tails[0], new Value.VEffectRow([.. unmatched], [.. tails]));
                return;
            case (0, _, 0):
                foreach (var tail in tails) ctx.Unify(tail, Value.VEffectRow.Pure);
                return;
            default:
                throw Unhandled(ctx, unmatched, inFunction);
        }
    }

    /// <summary>What a sink holds, as a row term read at the context's width.</summary>
    private static RowTerm RowOf(Context ctx, EffectSink performed) =>
        new([.. performed.Effects.Select(ctx.Quote)], [.. performed.Tails.Select(ctx.Quote)]);

    // ---- rows ---------------------------------------------------------------

    /// <summary>
    /// A written row, read in <paramref name="ctx"/> (under its arrow's binder). An
    /// entry is an effect, or - alone - a row variable; <c>_</c> is a fresh meta that
    /// must be solved by the entry. A bare arrow is pure (E3).
    /// </summary>
    private static RowTerm ElaborateRow(Context ctx, EffectRow? row)
    {
        if (row is null) return RowTerm.Pure;
        // A `~>` is rewritten where it sits in a signature (Elaborator.PolyArrows); one
        // that reaches here is outside any signature.
        if (row.Polymorphic) throw new FunException("a ~> arrow outside a signature: its row has nowhere to come from");

        var effects = new List<(Term Term, Value Value)>();
        var rowVariables = new List<Term>();
        foreach (var entry in row.Effects)
        {
            var (term, type) = Pure(ctx, c => Infer(c, entry));
            if (ctx.Force(type) is Value.VEffectRowTy)
            {
                rowVariables.Add(term);
                continue;
            }
            ctx.Unify(type, Value.VU.Instance);
            var value = ctx.Eval(term);
            if (ctx.Force(value) is not Value.VEffect) throw new FunException("expected an effect in an effect row");
            if (effects.Any(e => Nbe.SameEffect(ctx.Metas, e.Value, value))) throw new FunException("an effect row names an effect twice");
            effects.Add((term, value));
        }

        if (rowVariables.Count > 0 && (row.Inferred || effects.Count > 0 || !row.Tails.IsEmpty))
            throw new FunException("a row variable among effects is written after a bar: ->{E | r}");

        EquatableArray<Term> tails;
        if (rowVariables.Count > 0) tails = [.. rowVariables];
        else if (row.Inferred)
        {
            var meta = FreshRowMeta(ctx);
            ctx.Metas.WrittenRows.Add(((Term.InsertedMeta)meta).Id);
            tails = [meta];
        }
        else
            tails = [.. row.Tails.Select(t =>
            {
                var (term, type) = Pure(ctx, c => Infer(c, t));
                ctx.Unify(type, Value.VEffectRowTy.Instance);
                return term;
            })];

        return new RowTerm([.. effects.Select(e => e.Term)], tails);
    }

    // ---- declarations -----------------------------------------------------

    /// <summary>
    /// An effect family: each parameter a type, bound rigid over the operations; an
    /// operation's name written twice is an error. Its type is a type former over
    /// its parameters.
    /// </summary>
    private static (Term Decl, Value Type, Value Value) ElaborateEffectFamily(
        Context ctx, Id name, EquatableArray<Id> parameters, EquatableArray<EffectOp> ops)
    {
        var duplicate = ops.GroupBy(o => o.Name).FirstOrDefault(g => g.Count() > 1);
        if (duplicate is not null) throw new FunException($"an effect declares operation `{duplicate.Key}` twice");

        var paramCtx = parameters.Aggregate(ctx, (c, p) => c.Define(p.Name, Value.VU.Instance, new Value.VVar(c.Width, [])));
        var family = new EffectFamily(
            Interlocked.Increment(ref _nextEffectFamily), Label(name.Name), parameters.Length,
            [.. ops.Select(o => new EffectOperation(o.Name, TypeTerm(paramCtx, o.Input), TypeTerm(paramCtx, o.Output)))]);

        Term typeTerm = Term.U.Instance;
        for (var i = 0; i < parameters.Length; i++) typeTerm = new Term.Pi(Explicitness.Explicit, Term.U.Instance, typeTerm);
        var decl = new Term.EffectDecl(family);
        return (decl, ctx.Eval(typeTerm), ctx.Eval(decl));
    }

    private static (Term, Value) InferEffectDef(Context ctx, Syntax.EffectDef def)
    {
        var (decl, type, value) = ElaborateEffectFamily(ctx, def.Name, def.Params, def.Ops);
        var (body, bodyType) = Infer(ctx.Define(def.Name.Name, type, value), def.Body);
        return (new Term.Let(ctx.Quote(type), decl, body), bodyType);
    }

    /// <summary>An effect family as a module item: one slot holding the family.</summary>
    private static Context InferEffectBinding(Context ctx, Binding.Effect effect, List<BindingTerm> terms, List<ModuleEntry> entries)
    {
        var (decl, type, _) = ElaborateEffectFamily(ctx, effect.Name, effect.Params, effect.Ops);
        var kind = effect.Public ? MemberKind.Public : MemberKind.Private;
        var term = new BindingTerm.Let(Label(effect.Name.Name), kind, decl);
        terms.Add(term);
        entries.Add(new ModuleEntry.Field(term.Name, kind, type));
        return ExtendFromSlots(ctx, term, [(effect.Name.Name, type)]);
    }

    // ---- perform and resume -----------------------------------------------

    /// <summary>
    /// <c>E.op</c>: the effect instance the path without its last member names - a
    /// family's parameters not written become metas - and that operation's input
    /// and output types at the instance.
    /// </summary>
    private static (Term Instance, Value.VEffect Value, Value Input, Value Output) ResolveOperation(Context ctx, Syntax.FieldAccess operation)
    {
        var (term, type) = Pure(ctx, c => Infer(c, operation.Of));
        while (ctx.Force(type) is Value.VPi { Explicitness: Explicitness.Explicit } former)
        {
            var arg = FreshMeta(ctx);
            term = new Term.Ap(term, Explicitness.Explicit, arg);
            type = Nbe.ApplyClosure(ctx.Metas, former.Codomain, ctx.Eval(arg));
        }
        if (ctx.Force(ctx.Eval(term)) is not Value.VEffect instance) throw new FunException("expected an effect");
        var op = instance.Family.Operations.FirstOrDefault(o => o.Name == operation.Field)
            ?? throw new FunException($"unknown effect operation `{operation.Field}`");
        var (input, output) = Nbe.OperationTypes(ctx.Metas, instance, op);
        return (term, instance, input, output);
    }

    private static (Term, Value) InferPerform(Context ctx, Syntax.Perform perform)
    {
        var (instanceTerm, instance, input, output) = ResolveOperation(ctx, perform.Operation);
        var arg = Check(ctx, perform.Arg, input);
        Emit(ctx, [instance], []);
        return (new Term.Perform(instanceTerm, perform.Operation.Field, arg), ctx.Force(output));
    }

    /// <summary><c>resume(arg)</c>: applies the effect branch's continuation (E9).</summary>
    private static (Term, Value) InferResume(Context ctx, Syntax.Resume resume)
    {
        var entry = ctx.ResumeEntry ?? throw new FunException("resume is only available inside an effect branch");
        if (ctx.Force(entry.Type) is not Value.VPi { Explicitness: Explicitness.Explicit } cont)
            throw new InvalidOperationException("a continuation's type is a function");
        var arg = Check(ctx, resume.Arg, cont.Domain);
        var (index, _) = (Nbe.LevelToIndex(ctx.Width, entry.Level), entry.Type);
        var result = Nbe.ApplyClosure(ctx.Metas, cont.Codomain, ctx.Eval(arg));
        return (new Term.Ap(new Term.Var(index), Explicitness.Explicit, arg), ctx.Force(result));
    }

    // ---- application --------------------------------------------------------

    /// <summary>
    /// Calling a function performs its row. A row with an open tail may perform,
    /// through that tail, an effect it does not name; such a request belongs to the
    /// caller's caller, so the call tunnels past the handlers enclosing it here (E5).
    /// </summary>
    // ponytail: the row is read with a rigid argument, never the argument's value, so
    // a row naming its parameter (a reference's heap) arrives with refs.
    private static Term EmitLatent(Context ctx, Value.VPi pi, Term call)
    {
        if (pi.Row.Row.IsPure) return call;
        var row = Nbe.EvalRowClosure(ctx.Metas, pi.Row, new Value.VVar(ctx.Width, []));
        Emit(ctx, row.Effects, row.Tails);
        return row.Tails.IsEmpty || ctx.HandlerScopes.IsEmpty
            ? call
            : new Term.Tunnel([.. row.Effects.Select(ctx.Quote)], [.. ctx.HandlerScopes], call);
    }

    /// <summary>
    /// The argument's value, when evaluating it at check time is safe: it performs
    /// nothing. Otherwise a rigid stand-in.
    /// </summary>
    // ponytail: a codomain depending on an argument that performs reads the stand-in.
    private static Value ArgumentValue(Context ctx, Term arg, EffectSink argEffects) =>
        argEffects.IsEmpty ? ctx.Eval(arg) : new Value.VVar(ctx.Width, []);

    // ---- handlers -----------------------------------------------------------

    /// <summary>An effect branch resolved against what the handler's scrutinee performs.</summary>
    private sealed record HandlerBranch(MatchBranch Branch, Term InstanceTerm, Value.VEffect Instance, Value Input, Value Output);

    /// <summary>
    /// A branch's operation: its instance unified with an instance of the same family
    /// the scrutinee performs, when one is, so <c>effect State.get</c> handles the
    /// <c>State(I64)</c> performed.
    /// </summary>
    private static HandlerBranch ResolveHandlerBranch(Context ctx, EffectSink scrutineeEffects, MatchBranch branch)
    {
        var (term, instance, input, output) = ResolveOperation(ctx, branch.Operation!);
        foreach (var performed in scrutineeEffects.Effects)
            if (ctx.Force(performed) is Value.VEffect p && p.Family.Id == instance.Family.Id
                && Unify.TryValues(ctx.Metas, ctx.Width, instance, p))
                break;
        var forced = (Value.VEffect)ctx.Force(ctx.Eval(term));
        var op = forced.Family.Operations.First(o => o.Name == branch.Operation!.Field);
        var (i, o) = Nbe.OperationTypes(ctx.Metas, forced, op);
        return new HandlerBranch(branch, term, forced, i, o);
    }

    /// <summary>
    /// The instances a handler handles: those its scrutinee performs whose every
    /// operation has a branch. A branch for the same operation of the same instance
    /// twice is an error.
    /// </summary>
    private static List<Value> Handled(Context ctx, EffectSink scrutineeEffects, List<HandlerBranch> branches)
    {
        for (var i = 0; i < branches.Count; i++)
            for (var j = 0; j < i; j++)
                if (branches[i].Branch.Operation!.Field == branches[j].Branch.Operation!.Field
                    && Nbe.SameEffect(ctx.Metas, branches[i].Instance, branches[j].Instance))
                    throw new FunException($"duplicate effect branch for `{branches[i].Branch.Operation!.Field}`");

        return [.. scrutineeEffects.Effects.Where(performed =>
            ctx.Force(performed) is Value.VEffect p
            && p.Family.Operations.All(op => branches.Any(b =>
                b.Branch.Operation!.Field == op.Name && Nbe.SameEffect(ctx.Metas, b.Instance, p))))];
    }

    private static List<Value> Residual(Context ctx, IEnumerable<Value> performed, List<Value> handled) =>
        [.. performed.Where(e => !handled.Any(h => Nbe.SameEffect(ctx.Metas, e, h)))];

    /// <summary>
    /// An effect branch: its argument pattern (exhaustive) at the operation's input,
    /// then the continuation - from the operation's output to the match's result,
    /// performing what the handler leaves unhandled - and the body under both.
    /// </summary>
    private static EffectBranchTerm ElaborateEffectBranch(
        Context ctx, Context handlerCtx, HandlerBranch branch, Value resultType, List<Value> residual)
    {
        var (pattern, binders) = ElaboratePattern(ctx, branch.Branch.Pattern, branch.Input);
        var (tree, missing) = MatchCompile.Compile([pattern], occurrence => DomainOf(ctx, TypeAt(ctx, branch.Input, occurrence)));
        if (missing is not null) throw new FunException($"non-exhaustive effect branch pattern: {missing} is not matched");

        var argCtx = binders.Aggregate(handlerCtx, (c, b) => c.Bind(b.Name, b.Type));
        var contType = new Value.VPi(Explicitness.Explicit, branch.Output, new Closure(argCtx.Environment, argCtx.Quote(resultType).Shift(1)))
        {
            Row = new RowClosure(argCtx.Environment, new RowTerm([.. residual.Select(e => argCtx.Quote(e).Shift(1))], [])),
        };
        var (bodyCtx, contEntry) = argCtx.BindAnonymous(contType);
        var body = Check(bodyCtx with { ResumeEntry = contEntry }, branch.Branch.Body, resultType);
        return new EffectBranchTerm(ctx.Quote(branch.Instance), branch.Branch.Operation!.Field, tree!, body);
    }

    /// <summary>
    /// E6: a match's result may not carry a function whose row names an effect the
    /// match handles - the closure would escape its handler.
    /// </summary>
    private static void CheckEscape(Context ctx, List<Value> handled, Value type)
    {
        if (handled.Count == 0) return;
        string? Escaping(int level, Value ty)
        {
            switch (ctx.Force(ty))
            {
                case Value.VPi pi:
                    var x = new Value.VVar(level, []);
                    var row = Nbe.EvalRowClosure(ctx.Metas, pi.Row, x);
                    foreach (var e in row.Effects)
                        if (handled.Any(h => Nbe.SameEffect(ctx.Metas, e, h))) return ((Value.VEffect)ctx.Force(e)).Family.Name;
                    return Escaping(level, pi.Domain) ?? Escaping(level + 1, Nbe.ApplyClosure(ctx.Metas, pi.Codomain, x));
                case Value.VProdTy p:
                    return p.Items.Select(i => Escaping(level, i)).FirstOrDefault(n => n is not null);
                case Value.VModule m:
                    return m.Entries.OfType<ModuleEntry.Field>().Select(f => Escaping(level, f.Value)).FirstOrDefault(n => n is not null);
                default:
                    return null;
            }
        }
        if (Escaping(ctx.Width, type) is { } name)
            throw new FunException($"a function performing {name} escapes the handler that handles it");
    }

    private static int NextHandler() => Interlocked.Increment(ref _nextHandler);

    /// <summary>
    /// A meta for an inferred row: abstracted over the row variables in scope only,
    /// so a row may be solved to one while a value parameter never lands in its
    /// spine - a callback's row is the same row wherever it is called.
    /// </summary>
    private static Term FreshRowMeta(Context ctx)
    {
        var rowLevels = ctx.Names.Values
            .Where(e => ctx.Force(e.Type) is Value.VEffectRowTy)
            .Select(e => e.Level)
            .ToHashSet();
        var kinds = ctx.EntryKinds.Select((kind, index) =>
            kind == EntryKind.Bound && rowLevels.Contains(ctx.Width - 1 - index) ? EntryKind.Bound : EntryKind.Defined);
        return new Term.InsertedMeta(ctx.Metas.Fresh(), [.. kinds]);
    }
}
