using System.Reflection;
using Fun.Kernel;

namespace Fun.Tests;

/// <summary>
/// <see cref="Term.Map"/> is the one core-term traversal (the prototype's
/// <c>Core.map_subterms</c>): every de Bruijn walk reads it. A <see cref="Term"/>
/// or <see cref="BindingTerm"/> kind it does not cover is invisible until a
/// program reaches it, so these tests enumerate every kind by reflection and
/// make a new one fail here - C# has no exhaustiveness check for an open
/// record hierarchy, so a reflection test is the guard.
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
            var term = Sample(kind);
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

    private static List<Type> ConcreteKinds<T>() =>
        [.. typeof(T).Assembly.GetTypes()
            .Where(t => t.IsSubclassOf(typeof(T)) && !t.IsAbstract && t.IsNested)
            .OrderBy(t => t.Name)];

    /// <summary>A term of <paramref name="kind"/> whose subterm-valued fields are leaves.</summary>
    private static Term Sample(Type kind)
    {
        var constructor = kind.GetConstructors().Single();
        var arguments = constructor.GetParameters().Select(p => SampleArgument(p.ParameterType)).ToArray();
        return (Term)constructor.Invoke(arguments);
    }

    /// <summary>
    /// What <see cref="Sample"/> puts in a field: a leaf for the subterms <c>Map</c>
    /// walks, a decision tree it can read, and the type's default otherwise. A new
    /// kind with a field <c>Map</c> dereferences but this cannot build fails the test.
    /// </summary>
    private static object? SampleArgument(Type type) => type switch
    {
        _ when type == typeof(Term) => Term.U.Instance,
        _ when type == typeof(RowTerm) => RowTerm.Pure,
        _ when type == typeof(DecisionTree) => new DecisionTree.Leaf(0, []),
        _ when type == typeof(string) => "",
        _ when type.IsValueType => Activator.CreateInstance(type),
        _ => null,
    };
}
