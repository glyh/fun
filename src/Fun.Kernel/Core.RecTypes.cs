using System.Threading;

namespace Fun.Kernel;

/// <summary>
/// A <c>rec</c> struct type's declaration: the identity its recursive occurrences
/// share. Minted before the body is elaborated, so the body's references to the
/// type are occurrences of it; <see cref="Finish"/> records the finished body (a
/// struct type, or a function of the parameters to one) as a term in the
/// declaring environment, with the levels an occurrence captures (E11).
/// </summary>
// A class: two declarations with the same body are still two types.
public sealed class RecordDecl(string name)
{
    private static int _next;

    public int Id { get; } = Interlocked.Increment(ref _next);
    public string Name { get; } = name;

    public Environment? Environment { get; private set; }
    public Term? Body { get; private set; }
    public EquatableArray<int> Levels { get; private set; }

    /// <summary>Whether the body is known. Inside its own body an occurrence cannot unfold.</summary>
    public bool IsFinished => Body is not null;

    public void Finish(Environment environment, Term body, EquatableArray<int> levels)
    {
        if (IsFinished) throw new InvalidOperationException($"{this} is already finished");
        (Environment, Body, Levels) = (environment, body, levels);
    }

    public override string ToString() => $"{Name}#{Id}";
}

public abstract partial record Term
{
    /// <summary>
    /// A recursive occurrence: a <c>rec</c> struct type's reference to itself (or to
    /// a member of its group), by its declaration and the values of what its
    /// declaring scope names (<paramref name="Captures"/>), applied to its parameters.
    /// </summary>
    public sealed record RecursiveOccurrence(RecordDecl Decl, EquatableArray<Term> Captures, EquatableArray<Term> Args) : Term;
}

public abstract partial record Value
{
    /// <summary>
    /// A recursive occurrence: equal only to an occurrence of the same declaration,
    /// and unfolded to its struct type where a shape is needed.
    /// </summary>
    public sealed record VRecursiveOccurrence(RecordDecl Decl, EquatableArray<Value> Captures, EquatableArray<Value> Args) : Value;
}
