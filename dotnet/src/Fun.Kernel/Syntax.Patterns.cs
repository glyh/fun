namespace Fun.Kernel;

public abstract partial record Pattern
{
    /// <summary>
    /// <c>P {x = p, y, _}</c>: a record of the struct <paramref name="Type"/>, each
    /// named field matched by its pattern. <c>{y}</c> is <c>{y = y}</c>, a binder
    /// written by the label. <paramref name="Partial"/> (<c>_</c>) lets fields go unnamed.
    /// </summary>
    public sealed record Record(Syntax Type, EquatableArray<(string Name, Pattern Pattern)> Fields, bool Partial) : Pattern;
}
