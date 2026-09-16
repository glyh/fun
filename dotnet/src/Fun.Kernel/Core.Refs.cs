namespace Fun.Kernel;

/// <summary>A reference's cell: the one mutable thing in the semantic domain.</summary>
public sealed class RefCell(Value value)
{
    public Value Value { get; set; } = value;
}

/// <summary>
/// The mutation effect: <c>Mutate(h)</c>, parameterised by the heap a reference
/// lives on. Surface <c>Mutate(r)</c> names the reference and maps to its heap
/// (refs-in-effect-rows). It has no operations: the runtime handles it.
/// </summary>
public static class MutationEffect
{
    // An id no declaration mints: declared families count up from 1.
    public static readonly EffectFamily Family = new(-1, "Mutate", 1, []);

    public static Value On(Value heap) => new Value.VEffect(Family, Environment.Empty, [heap]);

    /// <summary>The heap a <c>Mutate</c> effect acts on, or null for any other effect.</summary>
    public static Value? HeapOf(Value effect) =>
        effect is Value.VEffect { Family.Id: -1 } e && e.Params.Length == 1 ? e.Params[0] : null;
}

public abstract partial record Term
{
    /// <summary><c>Ref(h, A)</c>: a reference on heap <paramref name="Heap"/> holding an <paramref name="Element"/>.</summary>
    public sealed record RefTy(Term Heap, Term Element) : Term;

    public sealed record RefNew(Term Arg) : Term;
    public sealed record RefGet(Term Ref) : Term;
    public sealed record RefSet(Term Ref, Term Value) : Term;
}

public abstract partial record Value
{
    public sealed record VRefTy(Value Heap, Value Element) : Value;

    /// <summary>A reference: equal to another exactly when it is the same cell.</summary>
    public sealed record VRef(RefCell Cell) : Value;
}

public abstract partial record Frame
{
    /// <summary><c>deref</c> of a stuck reference.</summary>
    public sealed record FRefGet : Frame
    {
        public static readonly FRefGet Instance = new();
    }

    /// <summary>A store into a stuck reference.</summary>
    public sealed record FRefSet(Value Value) : Frame;
}
