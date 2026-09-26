namespace Fun.Kernel;

public abstract partial record Syntax
{
    /// <summary>A block's <c>rec a = … and b = …</c>, scoped over the rest: every member sees every member.</summary>
    public sealed record LetRecGroup(EquatableArray<RecMember> Members, Syntax Body, SourceSpan Span) : Syntax(Span);
}

public abstract partial record Binding
{
    /// <summary><c>rec a = … and b = …</c> as a module item: every member sees every member.</summary>
    public sealed record RecGroup(EquatableArray<RecMember> Members, bool Public) : Binding;
}

/// <summary>One member of a <c>rec … and …</c> group; a typed member's value is annotated with its type.</summary>
public sealed record RecMember(Id Name, Syntax Value)
{
    public RecMember AddScope(ScopeSet scope) =>
        new(Name with { Scope = Name.Scope.Union(scope) }, Value.AddScope(scope));
}
