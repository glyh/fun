namespace Fun.Kernel;

public abstract partial record Pattern
{
    /// <summary>
    /// <c>P {x = p, y, _}</c>: a record of the struct <paramref name="Type"/>, each
    /// named field matched by its pattern. <c>{y}</c> is <c>{y = y}</c>, a binder
    /// written by the label. <paramref name="Partial"/> (<c>_</c>) lets fields go unnamed.
    /// </summary>
    public sealed record Record(Syntax Type, EquatableArray<(string Name, Pattern Pattern)> Fields, bool Partial) : Pattern;

    /// <summary>A primitive type head in a type-case: <c>I64</c>, <c>Unit</c>, <c>Char</c>, <c>String</c>, <c>Absurd</c>.</summary>
    public sealed record AtomType(AtomTy Ty) : Pattern;

    /// <summary>
    /// <c>struct { x : p; _ }</c> in a type-case: a struct type whose constructor
    /// fields' types match their patterns. Without <c>_</c> it has exactly these fields.
    /// </summary>
    public sealed record StructType(EquatableArray<(string Name, Pattern Pattern)> Fields, bool Partial) : Pattern;
}
