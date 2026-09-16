namespace Fun.Kernel;

public abstract partial record Syntax
{
    /// <summary>
    /// <c>struct { x : I64; pub f = … }</c>: a record type and a namespace in one --
    /// constructor fields alongside bindings, in source order.
    /// </summary>
    public sealed record Struct(EquatableArray<Binding> Bindings, SourceSpan Span) : Syntax(Span);

    /// <summary><c>P{x = 1; y = 2}</c>: a record of the struct <paramref name="Type"/>.</summary>
    public sealed record RecordConstruct(Syntax Type, EquatableArray<(string Name, Syntax Value)> Fields, SourceSpan Span)
        : Syntax(Span);

    /// <summary>
    /// <c>sig { T : Type; empty : T }</c>: a signature value. Each binding is a
    /// public member whose value is its type.
    /// </summary>
    public sealed record Sig(EquatableArray<Binding> Bindings, SourceSpan Span) : Syntax(Span);
}

public abstract partial record Binding
{
    /// <summary>
    /// A struct's constructor field <c>name : type</c>. A label, not a binder: the
    /// items after it do not see it.
    /// </summary>
    public sealed record Field(string Name, Syntax Type) : Binding;
}
