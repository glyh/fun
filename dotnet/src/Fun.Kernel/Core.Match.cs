namespace Fun.Kernel;

public abstract partial record Term
{
    /// <summary>
    /// A match, compiled once at elaboration: <paramref name="Tree"/> picks the
    /// arm and says where each of its binders sits in the scrutinee; arm
    /// <c>i</c>'s result is <c>Bodies[i]</c>, under its binders in source order.
    /// </summary>
    public sealed record Match(Term Scrutinee, EquatableArray<Term> Bodies, DecisionTree Tree) : Term
    {
        /// <summary>
        /// A match with effect branches is a handler: deep, lexical (E5, E8).
        /// <see cref="Handler"/> identifies it among the handlers a tunneled request skips.
        /// </summary>
        public EquatableArray<EffectBranchTerm> EffectBranches { get; init; } = [];

        public int Handler { get; init; }
    }
}

/// <summary>A pattern as elaboration leaves it: binders are positional, named by nothing.</summary>
public abstract partial record CorePattern
{
    public sealed record Wild : CorePattern
    {
        public static readonly Wild Instance = new();
    }

    public sealed record Bind : CorePattern
    {
        public static readonly Bind Instance = new();
    }

    public sealed record Atom(Fun.Kernel.Atom Value) : CorePattern;
    public sealed record Prod(EquatableArray<CorePattern> Items) : CorePattern;
    public sealed record Or(CorePattern Left, CorePattern Right) : CorePattern;

    /// <summary>
    /// A constructor, by its tag in the scrutinee's nominal. Its value's spine
    /// holds <paramref name="TypeParams"/> type arguments before the payloads.
    /// </summary>
    public sealed record Con(string Name, int TypeParams, EquatableArray<CorePattern> Args) : CorePattern;
}

/// <summary>A position inside a scrutinee.</summary>
public abstract record Occurrence
{
    public sealed record Base : Occurrence
    {
        public static readonly Base Instance = new();
    }

    /// <summary>The <paramref name="Index"/>th element of a tuple.</summary>
    public sealed record Child(Occurrence Parent, int Index) : Occurrence;

    /// <summary>
    /// The <paramref name="Index"/>th payload of the constructor <paramref name="Constructor"/>
    /// at <paramref name="Parent"/>: the tag says which payload types apply.
    /// </summary>
    public sealed record Payload(Occurrence Parent, string Constructor, int Index) : Occurrence;

    /// <summary>The field <paramref name="Name"/> of a record, or of a struct type (the field's type).</summary>
    public sealed record Field(Occurrence Parent, string Name) : Occurrence;
}

/// <summary>A compiled match: the tests to run on a scrutinee to choose an arm.</summary>
public abstract partial record DecisionTree
{
    /// <summary>Arm <paramref name="Branch"/> is chosen; its binders sit at <paramref name="Bindings"/>, in source order.</summary>
    public sealed record Leaf(int Branch, EquatableArray<Occurrence> Bindings) : DecisionTree;

    /// <summary>Branch on the constructor at <paramref name="At"/>.</summary>
    public sealed record Destruct(Occurrence At, EquatableArray<DestructCase> Cases, DecisionTree? Default) : DecisionTree;

    /// <summary>Branch on the atom at <paramref name="At"/>.</summary>
    public sealed record Switch(Occurrence At, EquatableArray<SwitchCase> Cases, DecisionTree Default) : DecisionTree;
}

public sealed record DestructCase(string Name, DecisionTree Tree);
public sealed record SwitchCase(Atom Key, DecisionTree Tree);
