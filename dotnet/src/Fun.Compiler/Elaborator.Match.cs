using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Elaborator
{
    /// <summary>
    /// A match: the scrutinee's type, refined by the first pattern that says
    /// something about it; each arm's result checked at one result type under
    /// its binders; the arms compiled to a decision tree, which fails exactly
    /// when some value no arm matches.
    /// </summary>
    private static (Term, Value) InferMatch(Context ctx, Syntax.Match match)
    {
        var (scrutinee, scrutineeType) = Infer(ctx, match.Scrutinee);
        scrutineeType = RefineScrutineeType(ctx, scrutineeType, match.Branches);
        var resultType = ctx.RawMeta();

        var patterns = new List<CorePattern>();
        var bodies = new List<Term>();
        foreach (var branch in match.Branches)
        {
            var (pattern, binders) = ElaboratePattern(ctx, branch.Pattern, scrutineeType);
            var inner = binders.Aggregate(ctx, (c, b) => c.Bind(b.Name, b.Type));
            patterns.Add(pattern);
            bodies.Add(Check(inner, branch.Body, resultType));
        }

        var (tree, missing) = MatchCompile.Compile(patterns, occurrence => DomainOf(ctx, TypeAt(ctx, scrutineeType, occurrence)));
        if (missing is not null) throw new FunException($"non-exhaustive match: {missing} is not matched");
        return (new Term.Match(scrutinee, [.. bodies], tree!), ctx.Force(resultType));
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
            Pattern.Con c => (ResolveConstructorHead(ctx, c.Head) ?? throw new NotImplementedException(BareConstructorHeadQuestion)).Nominal,
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

            case Pattern.Con c:
            {
                var (nominal, constructor) = ResolveConstructorHead(ctx, c.Head)
                    ?? throw new NotImplementedException(BareConstructorHeadQuestion);
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
        _ => throw new InvalidOperationException($"unhandled occurrence {occurrence.GetType().Name}"),
    };

    private static MatchDomain DomainOf(Context ctx, Value? type) => type is null
        ? MatchDomain.Unknown.Instance
        : ctx.Force(type) switch
        {
            Value.VAtomTy a => new MatchDomain.AtomDomain(a.Ty),
            Value.VNominal n => new MatchDomain.Nominal([.. n.Decl.Constructors.Select(c => new ConstructorShape(c.Name, 0, c.Payloads.Length))]),
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
