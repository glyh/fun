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

        // The members' entries have a width before they have values: predict each
        // member's captures in a context holding stand-ins for them.
        var standIns = members.Aggregate(ctx, (c, m) => c.Define(m.Name.Name, Value.VU.Instance, Value.VU.Instance))
            with { RecursiveLevels = recursive };
        var levels = shapes.Select(s => PredictCaptures(standIns, s.Lambdas, s.Enum)).ToList();

        var types = shapes.Select(s => ctx.Eval(FormerType(s.Lambdas))).ToList();
        var values = shapes.Select((s, i) =>
            Nbe.Eval(ctx.Metas, standIns.Environment, FormerTerm(decls[i], s.Lambdas.Count, levels[i], width + group, width + group))).ToList();

        var pending = ctx.PendingNominals;
        for (var i = 0; i < group; i++) pending = pending.SetItem(shapes[i].Enum, (decls[i], levels[i]));
        var inner = Enumerable.Range(0, group).Aggregate(ctx, (c, i) => c.Define(members[i].Name.Name, types[i], values[i]))
            with { RecursiveLevels = recursive, PendingNominals = pending };

        for (var i = 0; i < group; i++)
        {
            var (_, type) = Infer(inner, members[i].Value);
            if (!decls[i].IsComplete) throw new InvalidOperationException("a recursive enum's declaration was not completed");
            inner.Unify(types[i], type);
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
    /// The levels a member's enum will capture, worked out as <c>InferEnum</c> does
    /// from the context its lambdas build (each binding its parameter, its body
    /// the enclosing body), without elaborating it.
    /// </summary>
    private static EquatableArray<int> PredictCaptures(Context ctx, List<Syntax.Lam> lambdas, Syntax.Enum e)
    {
        foreach (var lam in lambdas)
            ctx = ctx.Bind(lam.Param.Name.Name, Value.VU.Instance) with { Enclosing = lam.Body };
        return FirstBoundLevel(ctx) is int firstBound
            ? NamedLevels(ctx, ctx.Enclosing)
                .Where(l => l >= firstBound && !ctx.RecursiveLevels.Contains(l))
                .Distinct()
                .Order()
                .ToEquatableArray()
            : [];
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

    /// <summary>A former's type: a <c>Type</c> parameter per lambda, then <c>Type</c>.</summary>
    private static Term FormerType(List<Syntax.Lam> lambdas) =>
        Enumerable.Reverse(lambdas).Aggregate((Term)Term.U.Instance,
            (acc, lam) => new Term.Pi(lam.Param.Explicitness, Term.U.Instance, acc));
}
