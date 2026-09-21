using Fun.Kernel;

namespace Fun.Tests;

/// <summary>
/// The one traversal of each tree - <see cref="Term.Map"/> (the prototype's
/// <c>Core.map_subterms</c>), <c>Syntax.Map</c> and <c>Binding.Map</c> - must
/// cover every kind. C# has no exhaustiveness check for an open record
/// hierarchy, so a kind a traversal forgets is invisible until a program
/// reaches it; these tests enumerate every kind by reflection and construct one,
/// so a new kind fails here until it is walked.
/// </summary>
public class CoreTraversalTests
{
    [Fact]
    public void Map_covers_every_term_kind()
    {
        var kinds = ConcreteKinds<Term>();
        Assert.NotEmpty(kinds);
        foreach (var kind in kinds)
        {
            var term = (Term)Sample(kind);
            var failure = Record.Exception(() => term.Map((_, _) => null));
            Assert.True(failure is null, $"{kind.Name}: {failure}");
        }
    }

    [Fact]
    public void MapBindings_covers_every_binding_term_kind()
    {
        BindingTerm[] samples =
        [
            new BindingTerm.Let("x", MemberKind.Public, Term.U.Instance),
            new BindingTerm.Open(Term.U.Instance, []),
            new BindingTerm.Impl("i", MemberKind.Public, Term.U.Instance, null!),
        ];
        var kinds = ConcreteKinds<BindingTerm>();
        Assert.Equal(kinds, [.. samples.Select(s => s.GetType()).OrderBy(t => t.Name)]);

        foreach (var sample in samples)
        {
            var module = new Term.Module([sample]);
            var failure = Record.Exception(() => module.Map((_, _) => null));
            Assert.True(failure is null, $"{sample.GetType().Name}: {failure}");
        }
    }

    [Fact]
    public void Map_covers_every_syntax_kind()
    {
        var kinds = ConcreteKinds<Syntax>();
        Assert.NotEmpty(kinds);
        foreach (var kind in kinds)
        {
            var syntax = (Syntax)Sample(kind);
            var failure = Record.Exception(() => syntax.Map(new SyntaxMapper()));
            Assert.True(failure is null, $"{kind.Name}: {failure}");
        }
    }

    [Fact]
    public void Map_covers_every_syntax_binding_kind()
    {
        var kinds = ConcreteKinds<Binding>();
        Assert.NotEmpty(kinds);
        foreach (var kind in kinds)
        {
            var binding = (Binding)Sample(kind);
            var failure = Record.Exception(() => binding.Map(new SyntaxMapper()));
            Assert.True(failure is null, $"{kind.Name}: {failure}");
        }
    }

    [Fact]
    public void Map_covers_every_pattern_kind()
    {
        var kinds = ConcreteKinds<Pattern>();
        Assert.NotEmpty(kinds);
        foreach (var kind in kinds)
        {
            var pattern = (Pattern)Sample(kind);
            var failure = Record.Exception(() => pattern.Map(new SyntaxMapper()));
            Assert.True(failure is null, $"{kind.Name}: {failure}");
        }
    }

    private static List<Type> ConcreteKinds<T>() =>
        [.. typeof(T).Assembly.GetTypes()
            .Where(t => t.IsSubclassOf(typeof(T)) && !t.IsAbstract && t.IsNested)
            .OrderBy(t => t.Name)];

    /// <summary>An instance of <paramref name="kind"/> whose child-valued fields are minimal.</summary>
    private static object Sample(Type kind)
    {
        var constructor = kind.GetConstructors().Single();
        var arguments = constructor.GetParameters().Select(p => SampleArgument(p.ParameterType)).ToArray();
        return constructor.Invoke(arguments);
    }

    /// <summary>
    /// What <see cref="Sample"/> puts in a field: a leaf the traversal can walk, or
    /// the type's default. A new kind with a field a traversal dereferences but this
    /// cannot build fails the test rather than passing silently.
    /// </summary>
    private static object? SampleArgument(Type type) => type switch
    {
        _ when type == typeof(Term) => Term.U.Instance,
        _ when type == typeof(RowTerm) => RowTerm.Pure,
        _ when type == typeof(DecisionTree) => new DecisionTree.Leaf(0, []),
        _ when type == typeof(Syntax) => Unit(),
        // A `perform`'s operation is a field access, not any syntax.
        _ when type == typeof(Syntax.FieldAccess) => new Syntax.FieldAccess(Unit(), "f", SourceSpan.Synthetic),
        _ when type == typeof(Binding) => new Binding.Let(new Id("x", SourceSpan.Synthetic), Unit(), false, false),
        _ when type == typeof(Pattern) => new Pattern.Bind(new Id("x", SourceSpan.Synthetic)),
        _ when type == typeof(Id) => new Id("x", SourceSpan.Synthetic),
        _ when type == typeof(Param) => new Param(new Id("x", SourceSpan.Synthetic), null, Explicitness.Explicit),
        _ when type == typeof(Role) => new Role(Fixity.Prefix, null, RoleMeaning.ApplyValue.Instance, SourceSpan.Synthetic, null),
        _ when type == typeof(Instantiation) => new Instantiation(new Id("x", SourceSpan.Synthetic), null!, [], null),
        _ when type == typeof(string) => "",
        _ when type == typeof(SourceSpan) => SourceSpan.Synthetic,
        _ when type.IsValueType => Activator.CreateInstance(type),
        _ => null,
    };

    private static Syntax Unit() => new Syntax.Atom(Atom.Unit.Instance, SourceSpan.Synthetic);
}
