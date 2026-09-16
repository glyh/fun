using System.Collections.Immutable;
using Fun.Kernel;

namespace Fun.Compiler;

/// <summary>What a position in a scrutinee can hold, as far as its type says.</summary>
public abstract record MatchDomain
{
    /// <summary>A nominal's constructors: tag, type-parameter count, payload count.</summary>
    public sealed record Nominal(EquatableArray<ConstructorShape> Constructors) : MatchDomain;

    public sealed record AtomDomain(AtomTy Ty) : MatchDomain;

    public sealed record Unknown : MatchDomain
    {
        public static readonly Unknown Instance = new();
    }
}

public sealed record ConstructorShape(string Name, int TypeParams, int Arity);

/// <summary>A value no arm matches, as far as the compiler can say.</summary>
public abstract record MissingPattern
{
    public sealed record Wild : MissingPattern
    {
        public static readonly Wild Instance = new();
    }

    public sealed record Con(string Name, MissingPattern? Payload) : MissingPattern;

    public sealed override string ToString() => this switch
    {
        Wild => "_",
        Con { Payload: null } c => c.Name,
        Con c => $"{c.Name}({c.Payload})",
        _ => throw new InvalidOperationException($"unhandled missing pattern {GetType().Name}"),
    };
}

/// <summary>
/// Compiles a match's patterns to a decision tree by specialising a clause
/// matrix column by column. A match that leaves some value unmatched does not
/// compile, and the compiler says which value.
/// </summary>
public static class MatchCompile
{
    private sealed record Row(ImmutableArray<CorePattern> Patterns, int Branch, ImmutableList<Occurrence> Bindings);

    private sealed record Matrix(ImmutableArray<Occurrence> Header, ImmutableList<Row> Rows);

    /// <summary>The tree, or what no arm matches.</summary>
    public static (DecisionTree? Tree, MissingPattern? Missing) Compile(
        IReadOnlyList<CorePattern> patterns, Func<Occurrence, MatchDomain> domainOf)
    {
        var initial = new Matrix(
            [Occurrence.Base.Instance],
            [.. patterns.Select((p, i) => new Row([p], i, []))]);
        return Go(initial, patterns, domainOf);
    }

    private static (DecisionTree?, MissingPattern?) Go(Matrix m, IReadOnlyList<CorePattern> source, Func<Occurrence, MatchDomain> domainOf)
    {
        if (m.Rows.IsEmpty) return (null, MissingPattern.Wild.Instance);

        var column = FindRefutableColumn(m);
        if (column < 0)
            return (new DecisionTree.Leaf(m.Rows[0].Branch, LeafBindings(m)), null);

        m = SwapColumns(m, 0, column);
        if (ExpandOrs(m) is { } expanded) return Go(expanded, source, domainOf);

        var occurrence = m.Header[0];
        var first = m.Rows.Select(r => r.Patterns[0]).First(p => p is not (CorePattern.Wild or CorePattern.Bind));
        switch (first)
        {
            case CorePattern.Prod prod:
                return Go(Specialize(m, prod.Items.Length, (p, arity) =>
                    p is CorePattern.Prod pr && pr.Items.Length == arity ? pr.Items : null), source, domainOf);

            case CorePattern.Con:
                return CompileDestruct(m, occurrence, source, domainOf);

            case CorePattern.Atom:
                return CompileSwitch(m, occurrence, source, domainOf);

            default:
                throw new InvalidOperationException($"unhandled pattern {first.GetType().Name}");
        }
    }

    private static (DecisionTree?, MissingPattern?) CompileDestruct(
        Matrix m, Occurrence occurrence, IReadOnlyList<CorePattern> source, Func<Occurrence, MatchDomain> domainOf)
    {
        var constructors = domainOf(occurrence) is MatchDomain.Nominal n ? n.Constructors : [];
        var tags = m.Rows.Select(r => r.Patterns[0]).OfType<CorePattern.Con>().Select(c => c.Name).Distinct().ToList();

        var cases = new List<DestructCase>();
        foreach (var tag in tags)
        {
            // The shape comes from the nominal; a pattern of that tag carries it too.
            var written = m.Rows.Select(r => r.Patterns[0]).OfType<CorePattern.Con>().First(c => c.Name == tag);
            var shape = constructors.FirstOrDefault(c => c.Name == tag) ?? new ConstructorShape(tag, written.TypeParams, written.Args.Length);
            var sub = SpecializeAt(m, shape.Arity, i => new Occurrence.Payload(occurrence, tag, shape.TypeParams + i),
                p => p is CorePattern.Con c && c.Name == tag ? c.Args : null);
            var (tree, missing) = Go(sub, source, domainOf);
            if (missing is not null) return (null, new MissingPattern.Con(tag, missing));
            cases.Add(new DestructCase(tag, tree!));
        }

        var missingCtors = constructors.Where(c => !tags.Contains(c.Name)).ToList();
        DecisionTree? fallback = null;
        if (missingCtors.Count > 0)
        {
            var dm = DefaultMatrix(m);
            if (dm.Rows.IsEmpty)
                return (null, new MissingPattern.Con(missingCtors[0].Name, missingCtors[0].Arity > 0 ? MissingPattern.Wild.Instance : null));
            var (tree, missing) = Go(dm, source, domainOf);
            if (missing is not null) return (null, missing);
            fallback = tree;
        }
        return (new DecisionTree.Destruct(occurrence, [.. cases], fallback), null);
    }

    private static (DecisionTree?, MissingPattern?) CompileSwitch(
        Matrix m, Occurrence occurrence, IReadOnlyList<CorePattern> source, Func<Occurrence, MatchDomain> domainOf)
    {
        var keys = m.Rows.Select(r => r.Patterns[0]).OfType<CorePattern.Atom>().Select(a => a.Value).Distinct().ToList();
        var cases = new List<SwitchCase>();
        foreach (var key in keys)
        {
            var (tree, missing) = Go(Specialize(m, 0, (p, _) => p is CorePattern.Atom a && a.Value == key ? [] : null), source, domainOf);
            if (missing is not null) return (null, missing);
            cases.Add(new SwitchCase(key, tree!));
        }

        var dm = DefaultMatrix(m);
        // Only Unit's and Absurd's atoms can be listed; every other atom type has more than any match names.
        var everyKeyCovered = domainOf(occurrence) is MatchDomain.AtomDomain { Ty: AtomTy.Unit or AtomTy.Absurd } d
            && (d.Ty == AtomTy.Absurd || keys.Contains(Atom.Unit.Instance));

        DecisionTree fallback;
        if (dm.Rows.IsEmpty)
        {
            if (!everyKeyCovered) return (null, MissingPattern.Wild.Instance);
            fallback = new DecisionTree.Leaf(m.Rows[0].Branch, []); // unreachable: every key has a case
        }
        else
        {
            var (tree, missing) = Go(dm, source, domainOf);
            if (missing is not null) return (null, missing);
            fallback = tree!;
        }
        return (new DecisionTree.Switch(occurrence, [.. cases], fallback), null);
    }

    private static int FindRefutableColumn(Matrix m)
    {
        for (var i = 0; i < m.Header.Length; i++)
            if (m.Rows.Any(r => r.Patterns[i] is not (CorePattern.Wild or CorePattern.Bind)))
                return i;
        return -1;
    }

    private static Matrix SwapColumns(Matrix m, int i, int j)
    {
        if (i == j) return m;
        ImmutableArray<T> Swap<T>(ImmutableArray<T> a) => a.SetItem(i, a[j]).SetItem(j, a[i]);
        return new Matrix(Swap(m.Header), [.. m.Rows.Select(r => r with { Patterns = Swap(r.Patterns) })]);
    }

    /// <summary>Rows whose first column is an or-pattern become one row per alternative; null when there are none.</summary>
    private static Matrix? ExpandOrs(Matrix m)
    {
        if (!m.Rows.Any(r => r.Patterns[0] is CorePattern.Or)) return null;
        return m with
        {
            Rows = [.. m.Rows.SelectMany(r => Alternatives(r.Patterns[0]).Select(alt => r with { Patterns = r.Patterns.SetItem(0, alt) }))],
        };
    }

    private static IEnumerable<CorePattern> Alternatives(CorePattern p) =>
        p is CorePattern.Or o ? Alternatives(o.Left).Concat(Alternatives(o.Right)) : [p];

    /// <summary>
    /// Replaces the first column by <paramref name="arity"/> child columns: a row
    /// whose pattern <paramref name="split"/> accepts contributes its
    /// sub-patterns, a wildcard or binder contributes wildcards (a binder binds
    /// the whole value first), any other row drops out.
    /// </summary>
    private static Matrix Specialize(Matrix m, int arity, Func<CorePattern, int, EquatableArray<CorePattern>?> split) =>
        SpecializeAt(m, arity, i => new Occurrence.Child(m.Header[0], i), p => split(p, arity));

    private static Matrix SpecializeAt(Matrix m, int arity, Func<int, Occurrence> child, Func<CorePattern, EquatableArray<CorePattern>?> split)
    {
        var at = m.Header[0];
        var header = Enumerable.Range(0, arity).Select(child).Concat(m.Header.Skip(1)).ToImmutableArray();
        var wilds = Enumerable.Repeat<CorePattern>(CorePattern.Wild.Instance, arity);
        var rows = m.Rows.SelectMany(r =>
        {
            var rest = r.Patterns.Skip(1);
            return r.Patterns[0] switch
            {
                CorePattern.Wild => [r with { Patterns = [.. wilds.Concat(rest)] }],
                CorePattern.Bind => [r with { Patterns = [.. wilds.Concat(rest)], Bindings = r.Bindings.Add(at) }],
                var p when split(p) is { } subs => [r with { Patterns = [.. subs.Concat(rest)] }],
                _ => Array.Empty<Row>(),
            };
        });
        return new Matrix(header, [.. rows]);
    }

    /// <summary>The rows that match whatever the first column holds, without it.</summary>
    private static Matrix DefaultMatrix(Matrix m) => new(
        [.. m.Header.Skip(1)],
        [.. m.Rows.SelectMany(r => r.Patterns[0] switch
        {
            CorePattern.Wild => [r with { Patterns = [.. r.Patterns.Skip(1)] }],
            CorePattern.Bind => [r with { Patterns = [.. r.Patterns.Skip(1)], Bindings = r.Bindings.Add(m.Header[0]) }],
            _ => Array.Empty<Row>(),
        })]);

    /// <summary>
    /// A leaf's binders in the order its arm writes them. Columns are resolved in
    /// whatever order the compiler picks, so the order they were collected in is
    /// not source order; source order is the occurrences' positional order.
    /// </summary>
    private static EquatableArray<Occurrence> LeafBindings(Matrix m)
    {
        var row = m.Rows[0];
        var remaining = m.Header.Where((_, i) => row.Patterns[i] is CorePattern.Bind);
        return [.. row.Bindings.Concat(remaining).OrderBy(Path, PathComparer.Instance)];
    }

    private static ImmutableList<int> Path(Occurrence o) => o switch
    {
        Occurrence.Base => [],
        Occurrence.Child c => Path(c.Parent).Add(c.Index),
        Occurrence.Payload p => Path(p.Parent).Add(p.Index),
        _ => throw new InvalidOperationException($"unhandled occurrence {o.GetType().Name}"),
    };

    private sealed class PathComparer : IComparer<ImmutableList<int>>
    {
        public static readonly PathComparer Instance = new();

        public int Compare(ImmutableList<int>? a, ImmutableList<int>? b)
        {
            for (var i = 0; i < Math.Min(a!.Count, b!.Count); i++)
                if (a[i] != b[i]) return a[i].CompareTo(b[i]);
            return a.Count.CompareTo(b.Count);
        }
    }
}
