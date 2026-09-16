namespace Fun.Kernel;

public abstract partial record Syntax
{
    /// <summary>
    /// <c>enum { Red, Some(A) }</c>: a nominal type value. Its constructors are
    /// members of the type, reached through it (<c>Color.Red</c>, <c>open Color</c>),
    /// so their names are labels rather than binders.
    /// </summary>
    public sealed record Enum(EquatableArray<EnumConstructor> Constructors, SourceSpan Span) : Syntax(Span);
}

public sealed record EnumConstructor(string Name, EquatableArray<Syntax> Payloads);
