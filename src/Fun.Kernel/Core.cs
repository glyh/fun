using System.Collections.Immutable;

namespace Fun.Kernel;

/// <summary>Whether an entry was introduced by a binder or by a definition.</summary>
// It is the mask a meta is abstracted over: the solver abstracts only
// over entries whose value is unknown.
public enum EntryKind { Bound, Defined }

/// <summary>
/// The core language: what elaboration produces and the evaluator runs.
/// Variables are de Bruijn indices -- the distance to their binder.
/// </summary>
// Only the constructors the current slice reaches are here; the rest of
// `Core.term` arrives as conformance cases demand them.
public abstract partial record Term
{
    public sealed record Var(int Index) : Term;
    public sealed record Lam(Term Body) : Term;
    public sealed record Ap(Term Fn, Explicitness Explicitness, Term Arg) : Term;

    /// <summary>let _ : A = Def in Body.</summary>
    public sealed record Let(Term Type, Term Def, Term Body) : Term;

    /// <summary>(x : Domain) ->{Row} Codomain. The row is read under the binder; absent, the arrow is pure.</summary>
    public sealed record Pi(Explicitness Explicitness, Term Domain, Term Codomain) : Term
    {
        public RowTerm Row { get; init; } = RowTerm.Pure;
    }

    /// <summary>Type : Type.</summary>
    public sealed record U : Term
    {
        public static readonly U Instance = new();
    }

    public sealed record Atom(Fun.Kernel.Atom Value) : Term;
    public sealed record AtomTy(Fun.Kernel.AtomTy Ty) : Term;

    /// <summary>A tuple: (a, b) has type ProdTy [A, B].</summary>
    public sealed record Prod(EquatableArray<Term> Items) : Term;
    public sealed record ProdTy(EquatableArray<Term> Items) : Term;
    public sealed record Proj(Term Of, int Index) : Term;

    /// <summary><c>e.name</c>: a member, by label. The last member of that name wins (I3).</summary>
    public sealed record Dot(Term Of, string Name) : Term;

    /// <summary>
    /// A module: each binding pushes its slots, and later bindings read earlier
    /// ones. A <paramref name="Signature"/>'s bindings are member types, and it
    /// evaluates to a partial module.
    /// </summary>
    public sealed record Module(EquatableArray<BindingTerm> Bindings, bool Signature = false) : Term;

    /// <summary>
    /// <c>open m in body</c>: pushes each of <paramref name="Members"/>, in order,
    /// before the body. Which members those are was decided from the module's type.
    /// </summary>
    public sealed record Open(Term Of, EquatableArray<OpenMember> Members, Term Body) : Term;

    /// <summary>A primitive, by name. It evaluates to a neutral headed by itself.</summary>
    public sealed record Prim(string Name) : Term;

    /// <summary>
    /// A residual meta, written by readback when it hits one still
    /// unsolved. The elaborator creates <see cref="InsertedMeta"/> instead.
    /// </summary>
    public sealed record Meta(int Id) : Term;

    /// <summary>
    /// A fresh meta as created. <paramref name="EntryKinds"/> records, per
    /// entry in scope, whether it was introduced by a binder or a definition;
    /// evaluation applies the meta to every bound one, so the solver abstracts
    /// only over entries whose value is unknown.
    /// </summary>
    public sealed record InsertedMeta(int Id, EquatableArray<EntryKind> EntryKinds) : Term;
}

/// <summary>
/// What a member of a module or struct is: a struct's constructor field, a
/// binding visible from outside or not, or a method visible from outside or not.
/// </summary>
public enum MemberKind { Public, Private, Field, Method, PrivateMethod }

/// <summary>What an <c>open</c> pushes: a member by label.</summary>
public abstract partial record OpenMember
{
    public sealed record Field(string Name) : OpenMember;
}

/// <summary>A binding as the core language holds it.</summary>
public abstract partial record BindingTerm
{
    public sealed record Let(string Name, MemberKind Kind, Term Def) : BindingTerm;

    /// <summary>
    /// An open inside a binding list. It adds no member; it pushes the opened
    /// members so the indices of the bindings after it line up.
    /// </summary>
    public sealed record Open(Term Of, EquatableArray<OpenMember> Members) : BindingTerm;

    /// <summary>
    /// What this binding contributes to a context, one slot per entry, in order.
    /// The elaborator and the evaluator both push exactly these (I2). Null for an
    /// open: its width is the opened module's public member count, known only
    /// once the module is elaborated, and carried by the term.
    /// </summary>
    public EquatableArray<Slot>? Slots() => this switch
    {
        Let l => [new Slot(l.Name, l.Kind, new SlotSource.Def(l.Def))],
        Impl i => [new Slot(i.Name, i.Kind, new SlotSource.Def(i.Def)) { ImplType = i.DictType }],
        Open => null,
        _ => throw new InvalidOperationException($"unhandled binding term {GetType().Name}"),
    };
}

/// <summary>One entry a binding adds, with the name it exports where it has one.</summary>
public sealed record Slot(string? Name, MemberKind Kind, SlotSource Source)
{
    /// <summary>For an impl's slot, its dictionary type: the entry it exports is an impl, not a field.</summary>
    public Value? ImplType { get; init; }
}

/// <summary>Where a slot's payload comes from; each side hangs its own payload on it.</summary>
public abstract partial record SlotSource
{
    /// <summary>Evaluate this term in the context so far.</summary>
    public sealed record Def(Term Term) : SlotSource;
}

/// <summary>
/// The semantic domain. Variables are de Bruijn *levels* -- the distance from
/// the outermost scope -- so a value stays meaningful as the context grows.
/// </summary>
public abstract partial record Value
{
    public sealed record VLam(Closure Body) : Value;

    public sealed record VPi(Explicitness Explicitness, Value Domain, Closure Codomain) : Value
    {
        /// <summary>The effects a call performs, read under the binder; pure unless written.</summary>
        public RowClosure Row { get; init; } = RowClosure.Pure;
    }

    public sealed record VU : Value
    {
        public static readonly VU Instance = new();
    }

    public sealed record VAtom(Fun.Kernel.Atom Atom) : Value;
    public sealed record VAtomTy(Fun.Kernel.AtomTy Ty) : Value;
    public sealed record VProd(EquatableArray<Value> Items) : Value;
    public sealed record VProdTy(EquatableArray<Value> Items) : Value;

    /// <summary>
    /// A module, or a module's type. Entries keep binding order; a dotted path
    /// takes the last entry of its name (I3). As a type, each entry holds its
    /// member's type.
    /// </summary>
    public sealed record VModule(EquatableArray<ModuleEntry> Entries, bool Partial) : Value
    {
        /// <summary>The last entry named <paramref name="name"/> that is visible from outside.</summary>
        public ModuleEntry.Field? PublicMember(string name) =>
            Entries.OfType<ModuleEntry.Field>().LastOrDefault(e => e.Name == name && e.Kind == MemberKind.Public);
    }

    /// <summary>
    /// A stuck computation: a primitive under elimination frames. Three kinds of
    /// stuck want three strategies, which is why they are three constructors --
    /// this one decomposes head and frames.
    /// </summary>
    public sealed record VNeutral(Value Ty, Head Head, EquatableArray<Frame> Frames) : Value;

    /// <summary>A meta applied to a spine; unification solves it.</summary>
    public sealed record VMeta(int Id, EquatableArray<Value> Spine) : Value;

    /// <summary>A bound variable applied to arguments; unification compares levels.</summary>
    public sealed record VVar(int Level, EquatableArray<Value> Spine) : Value;
}

public abstract partial record ModuleEntry
{
    public sealed record Field(string Name, MemberKind Kind, Value Value) : ModuleEntry
    {
        /// <summary>
        /// Set when the member is a constructor (an enum's, exported into the
        /// module): an open pushes it as a constructor entry, so a bare pattern
        /// head can resolve to it.
        /// </summary>
        public ConstructorMark? Constructor { get; init; }
    }
}

public abstract partial record Head
{
    public sealed record HVar(int Level) : Head;
    public sealed record HMeta(int Id) : Head;
    public sealed record HPrim(string Name) : Head;
}

/// <summary>An elimination waiting on a stuck value.</summary>
public abstract partial record Frame
{
    public sealed record FApp(Value Arg) : Frame;
    public sealed record FProj(int Index) : Frame;
    public sealed record FDot(string Name) : Frame;
}

/// <summary>A term under the environment it was written in.</summary>
public sealed record Closure(Environment Environment, Term Body);

/// <summary>
/// A scope's values, innermost first. The elaborator keeps this and its
/// <see cref="EntryKind"/> mask exactly the same length as its level; a term's de
/// Bruijn index counts entries here, so pushing a different number of entries
/// than the elaborator did is wrong quietly rather than loudly.
/// </summary>
public sealed class Environment
{
    private readonly ImmutableList<Value> _values;

    private Environment(ImmutableList<Value> values) => _values = values;

    public static readonly Environment Empty = new(ImmutableList<Value>.Empty);

    /// <summary>This scope with <paramref name="value"/> as its innermost entry.</summary>
    public Environment Push(Value value) => new(_values.Insert(0, value));

    /// <summary>The entry a de Bruijn index names.</summary>
    public Value this[int index] => _values[index];

    /// <summary>This environment with the entry at <paramref name="index"/> holding <paramref name="value"/> instead.</summary>
    public Environment Replace(int index, Value value) => new(_values.SetItem(index, value));

    public int Count => _values.Count;
}
