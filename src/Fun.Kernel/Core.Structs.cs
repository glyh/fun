namespace Fun.Kernel;

public abstract partial record Term
{
    /// <summary>
    /// A struct: its constructor fields' types, all read in the enclosing
    /// context, and its bindings, which push their slots like a module's.
    /// <paramref name="Partial"/> is a width-subtyped shape: any struct holding
    /// these members.
    /// </summary>
    public sealed record Struct(
        EquatableArray<(string Name, Term Type)> ConFields, EquatableArray<BindingTerm> Bindings, bool Partial) : Term;

    public sealed record RecordConstruct(Term Type, EquatableArray<(string Name, Term Value)> Fields) : Term;

    /// <summary>
    /// A signature: a telescope over the module it describes. <paramref name="Body"/>
    /// is a signature <see cref="Module"/> under one binder, the described module,
    /// so a member's type reads an earlier member as that module's member.
    /// </summary>
    public sealed record Sig(Term Body) : Term;
}

public abstract partial record Value
{
    /// <summary>
    /// A struct type. Entries keep binding order: constructor fields (kind
    /// <see cref="MemberKind.Field"/>) first, then the bindings' members.
    /// </summary>
    public sealed record VStruct(EquatableArray<ModuleEntry> Entries, bool Partial) : Value;

    /// <summary>A record: an instance of the struct <paramref name="Type"/>.</summary>
    public sealed record VRecord(Value Type, EquatableArray<(string Name, Value Value)> Fields) : Value;

    /// <summary>A signature: apply it to the module it describes to get that module's member types.</summary>
    public sealed record VSig(Closure Body) : Value;
}
