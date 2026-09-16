using System.Threading;

namespace Fun.Kernel;

/// <summary>
/// A nominal's declaration. Its identity is this object; a nominal type is the
/// declaration together with the values of its own free variables (its
/// captures), compared by conversion - applicative (E11).
/// </summary>
// A class, not a record: two declarations with the same constructors are still
// two types. Equality is reference equality.
public sealed class NominalDecl(string name, EquatableArray<ConstructorDecl> constructors, int captureCount)
{
    private static int _next;

    public int Id { get; } = Interlocked.Increment(ref _next);
    public string Name { get; } = name;

    /// <summary>Each constructor's payload types, as terms over the captures (capture <c>i</c> at level <c>i</c>).</summary>
    public EquatableArray<ConstructorDecl> Constructors { get; private set; } = constructors;

    public int CaptureCount { get; private set; } = captureCount;

    /// <summary>
    /// Whether the constructors are known. A recursive declaration is minted
    /// before its payloads are elaborated, since they name it.
    /// </summary>
    public bool IsComplete { get; private set; } = true;

    /// <summary>A declaration whose payloads are elaborated after it exists: <see cref="Complete"/> ties the knot.</summary>
    public static NominalDecl Declare(string name) => new(name, [], 0) { IsComplete = false };

    public void Complete(EquatableArray<ConstructorDecl> constructors, int captureCount)
    {
        if (IsComplete) throw new InvalidOperationException($"{this} is already complete");
        (Constructors, CaptureCount, IsComplete) = (constructors, captureCount, true);
    }

    /// <summary>A constructor by label: the last of that name, as for any member (I3).</summary>
    public ConstructorDecl? Constructor(string name) => Constructors.LastOrDefault(c => c.Name == name);

    public override string ToString() => $"{Name}#{Id}";
}

public sealed record ConstructorDecl(string Name, EquatableArray<Term> Payloads);

/// <summary>A constructor as a member carries it: the type (or former) it belongs to, that type's type, and which constructor.</summary>
public sealed record ConstructorMark(Value Type, Value TypeType, ConstructorDecl Constructor);

public abstract partial record Term
{
    /// <summary>The nominal of a declaration over the values of its captures.</summary>
    public sealed record Nominal(NominalDecl Decl, EquatableArray<Term> Captures) : Term;

    /// <summary>
    /// A constructor's value, at the end of the lambda chain that takes its
    /// payloads: <paramref name="Args"/> and <paramref name="Of"/> are variables of that chain.
    /// </summary>
    public sealed record Con(string Name, EquatableArray<Term> Args, Term Of) : Term;
}

public abstract partial record OpenMember
{
    /// <summary>
    /// A constructor of an opened type: the constructor itself for a nominal, or
    /// for a type former of <paramref name="FormerArity"/> parameters the
    /// constructor generic over them.
    /// </summary>
    public sealed record Constructor(string Name, int FormerArity) : OpenMember;
}

public abstract partial record Value
{
    public sealed record VNominal(NominalDecl Decl, EquatableArray<Value> Captures) : Value;

    /// <summary>A saturated constructor: its tag, its payloads, and the nominal it builds.</summary>
    public sealed record VCon(string Name, EquatableArray<Value> Args, VNominal Nominal) : Value;
}
