namespace Fun.Kernel;

public abstract partial record CorePattern
{
    /// <summary>A record's fields, by label, in the order written.</summary>
    public sealed record Record(EquatableArray<(string Name, CorePattern Pattern)> Fields, bool Partial) : CorePattern;

    /// <summary>A primitive type head.</summary>
    public sealed record AtomType(AtomTy Ty) : CorePattern;

    /// <summary>A struct type's constructor fields, by label; exactly these unless <paramref name="Partial"/>.</summary>
    public sealed record StructType(EquatableArray<(string Name, CorePattern Pattern)> Fields, bool Partial) : CorePattern;

    /// <summary>
    /// A nominal type head: the type <paramref name="Head"/> names - a term in the
    /// match's scope, a type former of <paramref name="Arity"/> parameters or a
    /// nominal - applied to types matching <paramref name="Params"/>. A type
    /// matches only if it is that instance: the declaration over convertible
    /// captures (E11), not merely the declaration.
    /// </summary>
    public sealed record NominalHead(NominalDecl Decl, Term Head, int Arity, EquatableArray<CorePattern> Params) : CorePattern;

    /// <summary>Whether matching this pattern needs more than a decision tree can test: a struct type's exact field set, or a nominal's instance.</summary>
    public bool NeedsDirectMatch() => this switch
    {
        StructType or NominalHead => true,
        Prod p => p.Items.Any(i => i.NeedsDirectMatch()),
        Or o => o.Left.NeedsDirectMatch() || o.Right.NeedsDirectMatch(),
        Con c => c.Args.Any(a => a.NeedsDirectMatch()),
        Record r => r.Fields.Any(f => f.Pattern.NeedsDirectMatch()),
        _ => false,
    };
}

/// <summary>A type-case key: a primitive type, or a nominal declaration.</summary>
public abstract record TypeKey
{
    public sealed record Atom(AtomTy Ty) : TypeKey;

    public sealed record Nominal(NominalDecl Decl) : TypeKey;
}

public sealed record TypeCase(TypeKey Key, DecisionTree Tree);

public abstract partial record DecisionTree
{
    /// <summary>
    /// Branch on the type at <paramref name="At"/>. A nominal case tests the
    /// declaration only; a match holding one runs as <see cref="Sequential"/>.
    /// </summary>
    public sealed record TypeSwitch(Occurrence At, EquatableArray<TypeCase> Cases, DecisionTree Default) : DecisionTree;

    /// <summary>
    /// Try each arm's pattern in order. For matches a tree cannot decide - a
    /// struct type's exact field set, a nominal type's instance - after the tree
    /// has checked them exhaustive.
    /// </summary>
    public sealed record Sequential(EquatableArray<CorePattern> Arms) : DecisionTree;
}
