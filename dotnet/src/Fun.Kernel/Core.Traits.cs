using System.Threading;

namespace Fun.Kernel;

/// <summary>
/// A trait's declaration. Its identity is this object: traits are nominal, so two
/// declarations with the same operations are still two traits.
/// </summary>
// A class, not a record: equality is reference equality.
public sealed class TraitDecl(string name, EquatableArray<(string Name, Closure Type)> operations)
{
    private static int _next;

    public int Id { get; } = Interlocked.Increment(ref _next);
    public string Name { get; } = name;

    /// <summary>
    /// Each operation's type, closed over the declaring environment and read under
    /// one more entry: the trait's argument.
    /// </summary>
    public EquatableArray<(string Name, Closure Type)> Operations { get; } = operations;

    public override string ToString() => $"{Name}#{Id}";
}

public abstract partial record Term
{
    /// <summary>A trait, by its declaration.</summary>
    public sealed record TraitRef(TraitDecl Decl) : Term;

    /// <summary>
    /// The dictionary type <c>Trait(Args)</c>: what an impl of the trait at these
    /// arguments provides, operation by operation.
    /// </summary>
    public sealed record TraitDictTy(TraitDecl Decl, EquatableArray<Term> Args, EquatableArray<(string Name, Term Type)> Operations) : Term;
}

public abstract partial record Value
{
    public sealed record VTrait(TraitDecl Decl) : Value;

    /// <summary>A trait dictionary type. An impl's value is a struct holding its operations.</summary>
    public sealed record VTraitDict(TraitDecl Decl, EquatableArray<Value> Args, EquatableArray<(string Name, Value Type)> Operations) : Value;
}

public abstract partial record BindingTerm
{
    /// <summary>
    /// An impl as a binding: one entry, its dictionary. <paramref name="Name"/> makes
    /// a named impl (<c>impl name : Trait(Arg)</c>) reachable as a member; an
    /// anonymous one arrives only through <c>open</c>. In a signature,
    /// <paramref name="Def"/> is the dictionary type the module must provide.
    /// </summary>
    public sealed record Impl(string? Name, MemberKind Kind, Term Def, Value DictType) : BindingTerm;
}

public abstract partial record ModuleEntry
{
    /// <summary>
    /// An impl among a module's or struct's entries, in binding order with the
    /// fields. As a module value it holds the dictionary; as a signature's
    /// instance, the dictionary type in both places.
    /// </summary>
    public sealed record Impl(string? Name, MemberKind Kind, Value DictType, Value Value) : ModuleEntry;
}

public abstract partial record OpenMember
{
    /// <summary>
    /// The <paramref name="Index"/>th public impl of the opened module, in entry
    /// order. A named one can also be projected by <paramref name="Name"/> when the
    /// module is not a known value (a signature-typed parameter).
    /// </summary>
    public sealed record Impl(int Index, string? Name) : OpenMember;
}
