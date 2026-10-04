namespace Quill.Kernel;

public abstract partial record CorePattern
{
    /// <summary>A record's fields, by label, in the order written.</summary>
    public sealed record Record(EquatableArray<(string Name, CorePattern Pattern)> Fields, bool Partial) : CorePattern;

    /// <summary>A pattern synonym's parameter <paramref name="Index"/>, replaced by the use's argument.</summary>
    public sealed record SynonymParam(int Index) : CorePattern;

    /// <summary>A primitive type head.</summary>
    public sealed record AtomType(AtomTy Ty) : CorePattern;

    /// <summary>
    /// A pin: the scrutinee must be convertible to <paramref name="Term"/>, a term
    /// of the match's scope. Its indices are rooted at <paramref name="Width"/>,
    /// the context width where it was elaborated (see <see cref="NominalHead.HeadWidth"/>),
    /// so the matcher re-roots it onto the run-time environment.
    /// </summary>
    public sealed record Pin(Term Term) : CorePattern
    {
        public int Width { get; init; }
    }

    /// <summary>A function type in a type-case: an <em>explicit</em> Pi, and one whose codomain does not depend on its domain.</summary>
    public sealed record Arrow(CorePattern Domain, CorePattern Codomain) : CorePattern;

    /// <summary>
    /// <c>[a] -&gt; b</c> in a type-case: an <em>implicit</em> Pi. The binder the
    /// pattern names is bound to a fresh rigid variable and the codomain matched
    /// at that variable; <paramref name="Codomain"/>'s mentions of the name are
    /// <see cref="Pin"/>s to it.
    /// </summary>
    public sealed record ImplicitArrow(CorePattern Codomain) : CorePattern;

    /// <summary>The universe in a type-case: a <see cref="TypeKey.U"/>.</summary>
    public sealed record Universe : CorePattern
    {
        public static readonly Universe Instance = new();
    }

    /// <summary>
    /// A tuple type pattern in a type-case - <c>(a, b)</c> and <c>Tuple(2, a, b)</c>
    /// are one form: <see cref="TypeKey.Tuple"/>, one component per item.
    /// </summary>
    public sealed record TupleType(EquatableArray<CorePattern> Items) : CorePattern;

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
        Wild or Atom or AtomType or Universe or Pin => 0,
        Or o => o.Left.Binders(),
        Prod p => p.Items.Sum(i => i.Binders()),
        Arrow a => a.Domain.Binders() + a.Codomain.Binders(),
        ImplicitArrow a => 1 + a.Codomain.Binders(),
        TupleType t => t.Items.Sum(i => i.Binders()),
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
        StructType or NominalHead or Pin => true,
        // An arrow is read off a Pi by dependence and explicitness, not by shape
        // alone, so both arrow forms take the ordered walk rather than the tree.
        Arrow or ImplicitArrow => true,
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

/// <summary>A type-case key: a primitive type, a nominal declaration, a function type, the universe, or a tuple type.</summary>
public abstract record TypeKey
{
    public sealed record Atom(AtomTy Ty) : TypeKey;

    public sealed record Nominal(NominalDecl Decl) : TypeKey;

    /// <summary>A function type, <c>A -&gt; B</c>: always two components.</summary>
    public sealed record Pi : TypeKey;

    /// <summary>The universe <c>Type</c>: no components.</summary>
    public sealed record U : TypeKey;

    /// <summary>A tuple type of <paramref name="Arity"/> components.</summary>
    public sealed record Tuple(int Arity) : TypeKey;
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
