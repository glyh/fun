using System.Collections.Immutable;
using Fun.Kernel;

namespace Fun.Compiler;

public sealed partial record Context
{
    /// <summary>
    /// The entries of the recursive type groups being elaborated. A nominal never
    /// captures them: it refers to them by its declaration.
    /// </summary>
    public ImmutableHashSet<int> RecursiveLevels { get; init; } = [];

    /// <summary>
    /// The declarations a recursive group minted before its members elaborate, by
    /// the <c>enum</c> each belongs to, with the captures its entry was built over.
    /// </summary>
    public ImmutableDictionary<Syntax.Enum, (NominalDecl Decl, EquatableArray<int> Levels)> PendingNominals { get; init; } =
        ImmutableDictionary.Create<Syntax.Enum, (NominalDecl, EquatableArray<int>)>(ReferenceEqualityComparer.Instance);
}

public static partial class Elaborator
{
    private enum RecKind { Value, Enum, Struct }

    /// <summary>What a <c>rec</c> member's value is once its type parameters are peeled.</summary>
    private static RecKind KindOf(Syntax value)
    {
        if (value is Syntax.Annotated a) value = a.Inner;
        while (value is Syntax.Lam lam) value = lam.Body;
        return value switch
        {
            Syntax.Enum => RecKind.Enum,
            Syntax.Struct => RecKind.Struct,
            _ => RecKind.Value,
        };
    }

    /// <summary>
    /// The kind every member of a <c>rec … and …</c> group shares. A group mixing
    /// enums, struct types and other values is an error.
    /// </summary>
    private static RecKind GroupKind(IEnumerable<Syntax> values)
    {
        var kinds = values.Select(KindOf).Distinct().ToList();
        if (kinds.Count > 1)
            throw new FunException("a rec … and … group holds enums, struct types or functions, not a mix");
        return kinds[0];
    }

    /// <summary>
    /// If <paramref name="e"/> belongs to a recursive group, completes the declaration
    /// its entry already refers to. Its captures must be the ones that entry was
    /// built over: they are predicted from the names in scope, and a payload type
    /// that reaches a further variable some other way is not ported.
    /// </summary>
    private static NominalDecl? CompletePending(
        Context ctx, Syntax.Enum e, EquatableArray<ConstructorDecl> constructors, EquatableArray<int> levels)
    {
        if (!ctx.PendingNominals.TryGetValue(e, out var pending)) return null;
        if (pending.Levels != levels)
            throw new NotImplementedException(
                "not ported yet: a recursive enum whose payload types capture a variable its body does not name");
        pending.Decl.Complete(constructors, levels.Length);
        return pending.Decl;
    }

    /// <summary>
    /// A group of recursive enums: <c>rec T = enum { … }</c>, a type former
    /// <c>rec L = fn(A : Type) { enum { … } }</c>, or <c>rec A = … and B = …</c>.
    /// Each member's declaration is minted first and every member is bound to its
    /// type (or former) over that declaration, so a payload naming a member means
    /// that nominal; the members then elaborate, completing their declarations.
    /// Member <c>i</c>'s term is at <paramref name="ctx"/>'s width plus <c>i</c>,
    /// where it is pushed.
    /// </summary>
    private static EquatableArray<(string Key, Term Term, Value Type, Value Value)> InferEnumGroup(
        Context ctx, EquatableArray<RecMember> members)
    {
        var width = ctx.Width;
        var group = members.Length;
        var recursive = ctx.RecursiveLevels.Union(Enumerable.Range(width, group));
        var shapes = members.Select(m => Shape(m.Value)).ToList();
        var decls = shapes.Select(_ => NominalDecl.Declare("enum")).ToList();
        ctx.Metas.DeclaredNominals.AddRange(decls);
        var types = shapes.Select(s => ctx.Eval(FormerType(s.Lambdas))).ToList();

        // The members' entries have a width before they have values: predict each
        // member's captures in a context holding stand-ins for them.
        var levels = PredictCaptures(ctx, members, types, decls, shapes, recursive);

        var values = shapes.Select((s, i) =>
            Nbe.Eval(ctx.Metas, StandInContext(ctx, members, types, decls, shapes, recursive, levels).Environment,
                FormerTerm(decls[i], s.Lambdas.Count, levels[i], width + group, width + group))).ToList();

        var pending = ctx.PendingNominals;
        for (var i = 0; i < group; i++) pending = pending.SetItem(shapes[i].Enum, (decls[i], levels[i]));
        var inner = Enumerable.Range(0, group).Aggregate(ctx, (c, i) => c.Define(members[i].Name.Name, types[i], values[i]))
            with { RecursiveLevels = recursive, PendingNominals = pending };

        for (var i = 0; i < group; i++)
        {
            // Elaborate the enum body over the declaration site, the former's
            // parameters bound on top of it - not through InferLam, which opens a
            // new enclosing scope and would drop the site's scope captures (a
            // generative module's stamp). The prototype's elab_type_group peels
            // the parameters the same way; this run completes the declaration's
            // constructors over the predicted captures. The former's type was
            // already predicted as types[i], so no re-unification is needed.
            var body = shapes[i].Lambdas.Aggregate(inner, (c, lam) => c.Bind(lam.Param.Name.Name, Value.VU.Instance));
            Infer(body, shapes[i].Enum);
            if (!decls[i].IsComplete) throw new InvalidOperationException("a recursive enum's declaration was not completed");
        }

        return [.. members.Select((m, i) => (m.Name.Name,
            FormerTerm(decls[i], shapes[i].Lambdas.Count, levels[i], width + i, width + group),
            types[i], values[i]))];
    }

    /// <summary>A member's type parameters (the lambdas around it) and its enum.</summary>
    private static (List<Syntax.Lam> Lambdas, Syntax.Enum Enum) Shape(Syntax value)
    {
        if (value is Syntax.Annotated a) value = a.Inner;
        var lambdas = new List<Syntax.Lam>();
        while (value is Syntax.Lam lam)
        {
            lambdas.Add(lam);
            value = lam.Body;
        }
        return (lambdas, (Syntax.Enum)value);
    }

    /// <summary>
    /// The context a recursive group's stand-ins live in: member <c>j</c> bound to
    /// its type and a former over its declaration with the captures
    /// <paramref name="levels"/> predicts for it, so a payload naming that member
    /// sees its captures. No former names a member's slot, so they evaluate in any
    /// environment the group's width deep - the stand-ins capturing nothing, say.
    /// </summary>
    private static Context StandInContext(
        Context ctx, EquatableArray<RecMember> members, List<Value> types, List<NominalDecl> decls,
        List<(List<Syntax.Lam> Lambdas, Syntax.Enum Enum)> shapes, ImmutableHashSet<int> recursive,
        List<EquatableArray<int>> levels)
    {
        var at = ctx.Width + members.Length;
        var flat = Enumerable.Range(0, members.Length).Aggregate(ctx,
            (c, j) => c.Define(members[j].Name.Name, types[j], Value.VU.Instance));
        return Enumerable.Range(0, members.Length).Aggregate(
            ctx with { RecursiveLevels = recursive },
            (c, j) => c.Define(members[j].Name.Name, types[j],
                Nbe.Eval(ctx.Metas, flat.Environment,
                    FormerTerm(decls[j], shapes[j].Lambdas.Count, levels[j], at, at))));
    }

    /// <summary>
    /// The levels each member's enum will capture, predicted before the members
    /// elaborate: exactly what <see cref="EnumCaptureLevels"/> computes at the
    /// use, read in a context holding stand-ins for the group. The payloads
    /// elaborate once, against stand-ins capturing nothing; the prediction starts
    /// from the levels their terms' variables stand at (as it always has) and then
    /// reads their values' levels under stand-ins with the captures predicted so
    /// far, repeating until the levels stop growing - a payload naming a member
    /// sees that member's captures, which grow with its own payloads' mentions.
    /// </summary>
    private static List<EquatableArray<int>> PredictCaptures(
        Context ctx, EquatableArray<RecMember> members, List<Value> types, List<NominalDecl> decls,
        List<(List<Syntax.Lam> Lambdas, Syntax.Enum Enum)> shapes, ImmutableHashSet<int> recursive)
    {
        var group = members.Length;
        // A former's parameters bind on top of the declaration site, but do not
        // open a new enclosing scope for capture purposes: the prototype's
        // elab_type_group captures the declaration site's scope_captures (a
        // generative module's stamp included), not the lambda body's. Resetting
        // Enclosing here would drop the stamp, so the group's own ctx supplies it.
        Context Body(Context standIns, List<Syntax.Lam> lambdas)
        {
            foreach (var lam in lambdas)
                standIns = standIns.Bind(lam.Param.Name.Name, Value.VU.Instance);
            return standIns;
        }

        var elaboration = StandInContext(ctx, members, types, decls, shapes, recursive,
            Enumerable.Repeat(EquatableArray<int>.Empty, group).ToList());
        var payloads = shapes.Select(s =>
        {
            var body = Body(elaboration, s.Lambdas);
            return (Body: body,
                Terms: s.Enum.Constructors.Select(c => c.Payloads.Select(p => TypeTerm(body, p)).ToList()).ToList());
        }).ToList();

        // The seed: the enclosing names and the levels the payload terms'
        // variables stand at - what the computation read before payload values
        // did - plus whatever the scope always captures. The rounds below only
        // grow it, so nothing captured before is lost.
        var levels = payloads.Select(p => (FirstBoundLevel(p.Body) is int firstBound
                ? NamedLevels(p.Body, p.Body.Enclosing)
                    .Concat(p.Terms.SelectMany(ps => ps.SelectMany(t => FreeLevels(p.Body, t))))
                    .Where(l => l >= firstBound && !p.Body.RecursiveLevels.Contains(l))
                : Enumerable.Empty<int>())
            .Concat(p.Body.ScopeCaptures)
            .Distinct()
            .Order()
            .ToEquatableArray()).ToList();

        // Levels only grow, and there are no more of them than the context has
        // entries, so the rounds are bounded; one that has not settled leaves the
        // disagreement for CompletePending to report.
        for (var round = 0; round < ctx.Width + group; round++)
        {
            var standIns = StandInContext(ctx, members, types, decls, shapes, recursive, levels);
            var next = payloads.Select((p, i) =>
            {
                var body = Body(standIns, shapes[i].Lambdas);
                return EnumCaptureLevels(body, p.Terms.SelectMany(ps => ps).Select(body.Eval));
            }).ToList();
            if (next.Zip(levels).All(x => x.First == x.Second)) return next;
            levels = next;
        }
        return levels;
    }

    /// <summary>
    /// <c>fn(A…) { T }</c> over a declaration, as a term at width <paramref name="at"/>.
    /// A captured level below the group is the variable it is there; a captured
    /// parameter was predicted past the whole group, at <paramref name="paramStart"/>
    /// onwards, and is the lambda's own variable.
    /// </summary>
    private static Term FormerTerm(NominalDecl decl, int arity, EquatableArray<int> levels, int at, int paramStart)
    {
        Term body = new Term.Nominal(decl, [.. levels.Select(l => (Term)new Term.Var(
            l >= paramStart ? arity - 1 - (l - paramStart) : Nbe.LevelToIndex(at + arity, l)))]);
        for (var j = 0; j < arity; j++) body = new Term.Lam(body);
        return body;
    }

    /// <summary>
    /// A group of recursive struct types: <c>rec L = struct { … }</c>, a former
    /// <c>rec L = fn(A : Type) { struct { … } }</c>, or <c>rec A = … and B = …</c>.
    /// Each binding mints a declaration; every member's body sees every member's
    /// name as a recursive occurrence of it (a function of the parameters to one)
    /// capturing what the enclosing scope names (E11). Member <c>i</c> elaborates
    /// where its binding sits, after the members before it are pushed, and its core
    /// binds the occurrences itself; its finished value is what an occurrence of it
    /// unfolds to.
    /// </summary>
    private static EquatableArray<(string Key, Term Term, Value Type, Value Value)> InferStructGroup(
        Context ctx, EquatableArray<RecMember> members)
    {
        var width = ctx.Width;
        var group = members.Length;
        var decls = members.Select(m => new RecordDecl(Label(m.Name.Name))).ToList();
        var arities = members.Select(m => Lambdas(m.Value).Count).ToList();
        var types = members.Select(m => ctx.Eval(FormerType(Lambdas(m.Value)))).ToList();
        var levels = (FirstBoundLevel(ctx) is int firstBound
            ? NamedLevels(ctx, ctx.Enclosing)
                .Where(l => l >= firstBound && l < width && !ctx.RecursiveLevels.Contains(l))
            : Enumerable.Empty<int>())
            .Concat(ctx.ScopeCaptures)
            .Distinct()
            .Order()
            .ToEquatableArray();

        var results = new List<(string, Term, Value, Value)>();
        var at = ctx;
        for (var i = 0; i < group; i++)
        {
            // The occurrences, bound on top of the members already pushed.
            var start = at.Width;
            var inner = Enumerable.Range(0, group).Aggregate(at, (c, j) =>
                c.Define(members[j].Name.Name, types[j], c.Eval(OccurrenceTerm(decls[j], arities[j], levels, c.Width))))
                with { RecursiveLevels = at.RecursiveLevels.Union(Enumerable.Range(start, group)) };

            var (body, type) = Infer(inner, members[i].Value);
            var core = Enumerable.Range(0, group).Reverse().Aggregate(body, (acc, j) =>
                new Term.Let(Nbe.Quote(ctx.Metas, start + j, types[j]), OccurrenceTerm(decls[j], arities[j], levels, start + j), acc));

            var value = at.Eval(core);
            decls[i].Finish(at.Environment, core, levels);
            results.Add((members[i].Name.Name, core, type, value));
            at = at.Define(members[i].Name.Name, type, value);
        }
        return [.. results];
    }

    private static List<Syntax.Lam> Lambdas(Syntax value)
    {
        if (value is Syntax.Annotated a) value = a.Inner;
        var lambdas = new List<Syntax.Lam>();
        while (value is Syntax.Lam lam)
        {
            lambdas.Add(lam);
            value = lam.Body;
        }
        return lambdas;
    }

    /// <summary>
    /// <c>fn(A…) { occurrence(A…) }</c>, as a term at width <paramref name="at"/>:
    /// each captured level is the variable it is there.
    /// </summary>
    private static Term OccurrenceTerm(RecordDecl decl, int arity, EquatableArray<int> levels, int at)
    {
        Term body = new Term.RecursiveOccurrence(decl,
            [.. levels.Select(l => (Term)new Term.Var(Nbe.LevelToIndex(at + arity, l)))],
            [.. Enumerable.Range(0, arity).Select(k => (Term)new Term.Var(arity - 1 - k))]);
        for (var j = 0; j < arity; j++) body = new Term.Lam(body);
        return body;
    }

    /// <summary>A former's type: a <c>Type</c> parameter per lambda, then <c>Type</c>.</summary>
    private static Term FormerType(List<Syntax.Lam> lambdas) =>
        Enumerable.Reverse(lambdas).Aggregate((Term)Term.U.Instance,
            (acc, lam) => new Term.Pi(lam.Param.Explicitness, Term.U.Instance, acc));
}
