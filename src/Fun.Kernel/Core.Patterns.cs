namespace Fun.Kernel;

public abstract partial record CorePattern
{
    /// <summary>A record's fields, by label, in the order written.</summary>
    public sealed record Record(EquatableArray<(string Name, CorePattern Pattern)> Fields, bool Partial) : CorePattern;

    /// <summary>A pattern synonym's parameter <paramref name="Index"/>, replaced by the use's argument.</summary>
    public sealed record SynonymParam(int Index) : CorePattern;

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
    public sealed record NominalHead(NominalDecl Decl, Term Head, int Arity, EquatableArray<CorePattern> Params) : CorePattern
{
    /// <summary>
    /// The context width where the head term was elaborated - its level base.
    /// A term's de Bruijn index names a level as width - 1 - index, and run-time
    /// environments are bottom-aligned with elaboration contexts (one entry per
    /// binding over the shared prelude), so the matcher re-roots the head with a
    /// constant shift of environment count minus this width and it resolves to
    /// the bindings it was written with, whatever is in scope at the match - a
    /// pattern synonym's template is elaborated at its definition, and a use
    /// site one binding further in must not shift what its head names.
    /// </summary>
    public int HeadWidth { get; init; }
}

    /// <summary>
    /// How many environment entries this pattern binds, in source order (an
    /// or-pattern's sides bind alike). The one statement of a pattern's binder
    /// count: the decision tree's leaves and a stuck arm both read it.
    /// </summary>
    public int Binders() => this switch
    {
        Bind => 1,
        Wild or Atom or AtomType => 0,
        Or o => o.Left.Binders(),
        Prod p => p.Items.Sum(i => i.Binders()),
        Con c => c.Args.Sum(a => a.Binders()),
        Record r => r.Fields.Sum(f => f.Pattern.Binders()),
        StructType s => s.Fields.Sum(f => f.Pattern.Binders()),
        NominalHead h => h.Params.Sum(p => p.Binders()),
        // A synonym's template is substituted away before it can be a match arm,
        // so a surviving parameter stands for an argument pattern nothing here knows.
        SynonymParam => throw new InvalidOperationException("a pattern synonym's template is not a match arm"),
        _ => throw new NotImplementedException($"not ported yet: the binders of a {GetType().Name} pattern"),
    };

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

public abstract partial record Term
{
    /// <summary>A pattern synonym: a closed value, as its definition elaborated it.</summary>
    public sealed record PatternSynonym(Value.VPatternSynonym Synonym) : Term;
}

public abstract partial record Value
{
    /// <summary>
    /// A pattern synonym: <paramref name="Rhs"/> matches a scrutinee of
    /// <paramref name="ScrutineeType"/>, with <see cref="CorePattern.SynonymParam"/>
    /// where each parameter sits. <paramref name="Params"/> gives each parameter's
    /// type, in the order the parameters sit in the scrutinee - the order their
    /// binders come out in. Where the right-hand side cannot determine its types,
    /// those metas are <paramref name="Generalized"/> (<paramref name="TypeParams"/>
    /// of them), instantiated afresh at each use; <paramref name="Env"/> and
    /// <paramref name="Width"/> are the definition's, so the names it captures stay
    /// the definition's while only the generalized types come from the use.
    /// </summary>
    // Compared by reference: two synonyms are the same only if they are one definition.
    public sealed record VPatternSynonym(
        int Arity, int TypeParams, EquatableArray<int> Generalized,
        CorePattern Rhs, Value ScrutineeType, EquatableArray<(int Index, Value Type)> Params,
        Environment Env, int Width) : Value
    {
        public bool Equals(VPatternSynonym? other) => ReferenceEquals(this, other);
        public override int GetHashCode() => System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(this);
    }
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
