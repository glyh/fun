using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Elaborator
{
    private static (Term, Value) InferMatch(Context ctx, Syntax.Match match) => ElaborateMatch(ctx, match, expected: null);

    private static Term CheckMatch(Context ctx, Syntax.Match match, Value expected) => ElaborateMatch(ctx, match, expected).Item1;

    /// <summary>
    /// A match: the scrutinee's type, refined by the first pattern that says
    /// something about it; each arm's result checked at one result type under
    /// its binders; the arms compiled to a decision tree, which fails exactly
    /// when some value no arm matches. A type-case on a type variable narrows
    /// that variable to each branch's type head, in the branch's context and -
    /// when checking - in its expected type.
    /// </summary>
    private static (Term, Value) ElaborateMatch(Context ctx, Syntax.Match match, Value? expected)
    {
        // A match with effect branches is a handler: its scrutinee and branch bodies
        // elaborate inside it, for tunneling (E5).
        var (storedBefore, since) = (ctx.Sink.Stored.Count, ctx.Metas.Count);
        var effectSyntax = match.Branches.Where(b => b.Operation is not null).ToList();
        var valueBranches = new EquatableArray<MatchBranch>([.. match.Branches.Where(b => b.Operation is null)]);
        var handler = effectSyntax.Count > 0 ? NextHandler() : 0;
        var hctx = handler == 0 ? ctx : ctx with { HandlerScopes = ctx.HandlerScopes.Add(handler) };

        var ((scrutinee, scrutineeType), scrutineeEffects) = Collecting(hctx, c => Infer(c, match.Scrutinee));
        var effectBranches = effectSyntax.Select(b => ResolveHandlerBranch(ctx, scrutineeEffects, b)).ToList();
        var handled = Handled(ctx, scrutineeEffects, effectBranches);
        var residual = Residual(ctx, scrutineeEffects.Effects, handled);

        scrutineeType = RefineScrutineeType(ctx, scrutineeType, valueBranches);
        var resultType = expected ?? ctx.RawMeta();
        var target = RefinementTarget(ctx, scrutinee, scrutineeType);

        var patterns = new List<CorePattern>();
        var bodies = new List<Term>();
        var ((branchTerms, _), bodyEffects) = Collecting(hctx, bctx =>
        {
            foreach (var branch in valueBranches)
            {
                var (branchCtx, branchExpected) = (bctx, resultType);
                if (target is int level && RefinementOf(bctx, branch.Pattern) is { } replacement)
                {
                    branchCtx = RefineContext(bctx, level, replacement);
                    if (expected is not null) branchExpected = Substitute(bctx, level, replacement, expected);
                }
                var (pattern, binders) = ElaboratePattern(branchCtx, branch.Pattern, scrutineeType);
                var inner = binders.Aggregate(branchCtx, (c, b) => c.Bind(b.Name, b.Type));
                patterns.Add(pattern);
                bodies.Add(Check(inner, branch.Body, branchExpected));
            }
            return (effectBranches.Select(b => ElaborateEffectBranch(ctx, bctx, b, resultType, residual)).ToList(), 0);
        });

        // A handler is deep (E8): what its branch bodies perform it handles too.
        var all = new EffectSink();
        all.Effects.AddRange(scrutineeEffects.Effects.Concat(bodyEffects.Effects));
        Emit(ctx, Residual(ctx, all.Effects, Handled(ctx, all, effectBranches)), scrutineeEffects.Tails.Concat(bodyEffects.Tails));
        // E6 is about the instances the branches name, handled in full or not.
        CheckEscape(ctx, [.. effectBranches.Select(b => (Value)b.Instance)], resultType);
        CheckStoredEscape(ctx, [.. effectBranches.Select(b => (Value)b.Instance)], storedBefore, since);

        var (tree, missing) = MatchCompile.Compile(patterns, occurrence => DomainOf(ctx, TypeAt(ctx, scrutineeType, occurrence)));
        if (missing is not null) throw new FunException($"non-exhaustive match: {missing} is not matched");
        // A struct type's exact field set and a nominal type's instance are more
        // than a tree tests: such a match runs its arms in order, once the tree
        // has shown them exhaustive.
        var run = patterns.Any(p => p.NeedsDirectMatch()) ? new DecisionTree.Sequential([.. patterns]) : tree!;
        return (new Term.Match(scrutinee, [.. bodies], run) { EffectBranches = [.. branchTerms], Handler = handler }, ctx.Force(resultType));
    }

    /// <summary>
    /// A scrutinee whose type is not yet a tuple, atom or nominal takes the type
    /// the first informative pattern implies.
    /// </summary>
    private static Value RefineScrutineeType(Context ctx, Value type, EquatableArray<MatchBranch> branches)
    {
        type = ctx.Force(type);
        if (type is Value.VAtomTy or Value.VProdTy or Value.VNominal) return type;

        Value? Implied(Pattern p) => p switch
        {
            Pattern.Atom a => new Value.VAtomTy(AtomTypeOf(a.Value)),
            Pattern.Prod prod => new Value.VProdTy([.. prod.Items.Select(i => Implied(i) ?? ctx.RawMeta())]),
            Pattern.Or o => Implied(o.Left) ?? Implied(o.Right),
            Pattern.Con c when SynonymAt(ctx, c.Head) is { } synonym => synonym.ScrutineeType,
            // A head naming a type makes this a type-case: the scrutinee is a type.
            Pattern.Con c => TypeHead(ctx, c.Head) is not null
                ? Value.VU.Instance
                : (ResolveConstructorHead(ctx, c.Head) ?? throw new NotImplementedException(UnportedConstructorHead)).Nominal,
            Pattern.Record r => RecordPatternType(ctx, r),
            Pattern.AtomType => Value.VU.Instance,
            Pattern.StructType => type is Value.VStruct ? type : Value.VU.Instance,
            _ => null,
        };

        foreach (var branch in branches)
        {
            if (Implied(branch.Pattern) is not { } implied) continue;
            ctx.Unify(type, implied);
            return ctx.Force(type);
        }
        return type;
    }

    /// <summary>A pattern against a scrutinee type, with the binders it adds, in source order.</summary>
    private static (CorePattern, List<(string Name, Value Type)>) ElaboratePattern(Context ctx, Pattern pattern, Value type)
    {
        switch (pattern)
        {
            case Pattern.Wild:
                return (CorePattern.Wild.Instance, []);

            case Pattern.Bind b:
                return (CorePattern.Bind.Instance, [(b.Name.Name, type)]);

            case Pattern.Atom a:
                ctx.Unify(type, new Value.VAtomTy(AtomTypeOf(a.Value)));
                return (new CorePattern.Atom(a.Value), []);

            case Pattern.Or o:
            {
                var (left, leftBinders) = ElaboratePattern(ctx, o.Left, type);
                var (right, rightBinders) = ElaboratePattern(ctx, o.Right, type);
                if (!leftBinders.Select(b => b.Name).SequenceEqual(rightBinders.Select(b => b.Name)))
                    throw new FunException("the alternatives of an or-pattern bind different names");
                foreach (var (l, r) in leftBinders.Zip(rightBinders)) ctx.Unify(l.Type, r.Type);
                return (new CorePattern.Or(left, right), leftBinders);
            }

            case Pattern.Prod prod:
            {
                if (ctx.Force(type) is not Value.VProdTy tuple || tuple.Items.Length != prod.Items.Length)
                    throw new FunException("tuple length mismatch");
                var items = new List<CorePattern>();
                var binders = new List<(string, Value)>();
                foreach (var (item, itemType) in prod.Items.Zip(tuple.Items))
                {
                    var (core, itemBinders) = ElaboratePattern(ctx, item, itemType);
                    items.Add(core);
                    binders.AddRange(itemBinders);
                }
                return (new CorePattern.Prod([.. items]), binders);
            }

            case Pattern.Con c when SynonymAt(ctx, c.Head) is { } synonym:
                return ElaborateSynonymUse(ctx, c, synonym, type);

            case Pattern.SynonymParam p:
                return (new CorePattern.SynonymParam(p.Index), [($"{SynonymParamPrefix}{p.Index}", type)]);

            case Pattern.Con c when ctx.Force(type) is Value.VU:
                return ElaborateNominalHeadPattern(ctx, c);

            case Pattern.Con c:
            {
                var (nominal, constructor) = ResolveConstructorHead(ctx, c.Head)
                    ?? throw new NotImplementedException(UnportedConstructorHead);
                ctx.Unify(type, nominal);
                if (ctx.Force(type) is not Value.VNominal scrutinee)
                    throw new InvalidOperationException("a scrutinee unified with a nominal is one");
                if (c.Args.Length != constructor.Payloads.Length)
                    throw new FunException($"`{constructor.Name}` takes {constructor.Payloads.Length} payloads, the pattern gives {c.Args.Length}");
                var args = new List<CorePattern>();
                var binders = new List<(string, Value)>();
                foreach (var (arg, argType) in c.Args.Zip(Nbe.PayloadTypes(ctx.Metas, scrutinee, constructor)))
                {
                    var (core, argBinders) = ElaboratePattern(ctx, arg, argType);
                    args.Add(core);
                    binders.AddRange(argBinders);
                }
                return (new CorePattern.Con(constructor.Name, 0, [.. args]), binders);
            }

            case Pattern.Record r:
                return ElaborateRecordPattern(ctx, r, type);

            case Pattern.AtomType t:
                ctx.Unify(type, Value.VU.Instance);
                return (new CorePattern.AtomType(t.Ty), []);

            case Pattern.StructType s:
                return ElaborateStructTypePattern(ctx, s, type);

            default:
                throw new InvalidOperationException($"unhandled pattern {pattern.GetType().Name}");
        }
    }

    /// <summary>The type at a position inside a scrutinee of <paramref name="type"/>, where its type says.</summary>
    private static Value? TypeAt(Context ctx, Value type, Occurrence occurrence) => occurrence switch
    {
        Occurrence.Base => type,
        Occurrence.Payload p => TypeAt(ctx, type, p.Parent) is { } parent && ctx.Force(parent) is Value.VNominal n
                                && n.Decl.Constructor(p.Constructor) is { } constructor
            ? Nbe.PayloadTypes(ctx.Metas, n, constructor)[p.Index]
            : null,
        Occurrence.Child c => TypeAt(ctx, type, c.Parent) is { } parent && ctx.Force(parent) is Value.VProdTy tuple
                              && c.Index < tuple.Items.Length
            ? tuple.Items[c.Index]
            : null,
        Occurrence.Field f => TypeAt(ctx, type, f.Parent) is { } parent && ctx.Force(parent) is Value.VStruct st
            ? FieldType(st, f.Name)
            : null,
        _ => throw new InvalidOperationException($"unhandled occurrence {occurrence.GetType().Name}"),
    };

    private static MatchDomain DomainOf(Context ctx, Value? type) => type is null
        ? MatchDomain.Unknown.Instance
        : ctx.Force(type) switch
        {
            Value.VAtomTy a => new MatchDomain.AtomDomain(a.Ty),
            Value.VNominal n => new MatchDomain.Nominal([.. n.Decl.Constructors.Select(c => new ConstructorShape(c.Name, 0, c.Payloads.Length))]),
            Value.VStruct st => new MatchDomain.Record([.. st.Entries.OfType<ModuleEntry.Field>().Where(f => f.Kind == MemberKind.Field).Select(f => f.Name).Distinct()]),
            _ => MatchDomain.Unknown.Instance,
        };

    private static AtomTy AtomTypeOf(Atom atom) => atom switch
    {
        Atom.I64 => AtomTy.I64,
        Atom.Unit => AtomTy.Unit,
        Atom.Char => AtomTy.Char,
        Atom.Str => AtomTy.String,
        Atom.Scopes => AtomTy.Scopes,
        _ => throw new InvalidOperationException($"unhandled atom {atom.GetType().Name}"),
    };
}
