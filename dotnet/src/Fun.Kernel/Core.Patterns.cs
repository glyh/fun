namespace Fun.Kernel;

public abstract partial record CorePattern
{
    /// <summary>A record's fields, by label, in the order written.</summary>
    public sealed record Record(EquatableArray<(string Name, CorePattern Pattern)> Fields, bool Partial) : CorePattern;
}
