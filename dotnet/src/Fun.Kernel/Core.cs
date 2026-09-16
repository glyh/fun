using System.Collections.Immutable;

namespace Fun.Kernel;

/// <summary>Whether an entry was introduced by a binder or by a definition.</summary>
// It is the mask a metavariable is abstracted over: the solver abstracts only
// over entries whose value is unknown.
public enum Bd { Bound, Defined }

/// <summary>
/// The core language: what elaboration produces and the evaluator runs.
/// Variables are de Bruijn indices -- the distance to their binder.
/// </summary>
// Only the constructors the current slice reaches are here; the rest of
// `Core.term` arrives as conformance cases demand them.
public abstract record Term
{
    public sealed record Var(int Index) : Term;
    public sealed record Lam(Term Body) : Term;
    public sealed record Ap(Term Fn, Explicitness Explicitness, Term Arg) : Term;

    /// <summary>let _ : A = Def in Body.</summary>
    public sealed record Let(Term Type, Term Def, Term Body) : Term;

    /// <summary>(x : Domain) -> Codomain. A row is not carried yet: every arrow is pure.</summary>
    public sealed record Pi(Explicitness Explicitness, Term Domain, Term Codomain) : Term;

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

    /// <summary>A primitive, by name. It evaluates to a neutral headed by itself.</summary>
    public sealed record Prim(string Name) : Term;

    /// <summary>
    /// A residual metavariable, written by readback when it hits one still
    /// unsolved. The elaborator creates <see cref="InsertedMeta"/> instead.
    /// </summary>
    public sealed record Meta(int Id) : Term;

    /// <summary>
    /// A fresh metavariable as created. <paramref name="Bds"/> records, per
    /// entry in scope, whether it was introduced by a binder or a definition;
    /// evaluation applies the meta to every bound one, so the solver abstracts
    /// only over entries whose value is unknown.
    /// </summary>
    public sealed record InsertedMeta(int Id, EquatableArray<Bd> Bds) : Term;
}

/// <summary>
/// The semantic domain. Variables are de Bruijn *levels* -- the distance from
/// the outermost scope -- so a value stays meaningful as the context grows.
/// </summary>
public abstract record Value
{
    public sealed record VLam(Closure Body) : Value;

    public sealed record VPi(Explicitness Explicitness, Value Domain, Closure Codomain) : Value;

    public sealed record VU : Value
    {
        public static readonly VU Instance = new();
    }

    public sealed record VAtom(Fun.Kernel.Atom Atom) : Value;
    public sealed record VAtomTy(Fun.Kernel.AtomTy Ty) : Value;
    public sealed record VProd(EquatableArray<Value> Items) : Value;
    public sealed record VProdTy(EquatableArray<Value> Items) : Value;

    /// <summary>
    /// A stuck computation: a primitive under elimination frames. Three kinds of
    /// stuck want three strategies, which is why they are three constructors --
    /// this one decomposes head and frames.
    /// </summary>
    public sealed record VNeutral(Value Ty, Head Head, EquatableArray<Frame> Frames) : Value;

    /// <summary>A metavariable applied to a spine; unification solves it.</summary>
    public sealed record VFlex(int Id, EquatableArray<Value> Spine) : Value;

    /// <summary>A bound variable applied to arguments; unification compares levels.</summary>
    public sealed record VRigid(int Level, EquatableArray<Value> Spine) : Value;
}

public abstract record Head
{
    public sealed record HVar(int Level) : Head;
    public sealed record HMeta(int Id) : Head;
    public sealed record HPrim(string Name) : Head;
}

/// <summary>An elimination waiting on a stuck value.</summary>
public abstract record Frame
{
    public sealed record FApp(Value Arg) : Frame;
    public sealed record FProj(int Index) : Frame;
}

/// <summary>A term under the environment it was written in.</summary>
public sealed record Closure(Env Env, Term Body);

/// <summary>
/// A scope's values, innermost first. The elaborator keeps this and its
/// <see cref="Bd"/> mask exactly the same length as its level; a term's de
/// Bruijn index counts entries here, so pushing a different number of entries
/// than the elaborator did is wrong quietly rather than loudly.
/// </summary>
public sealed class Env
{
    private readonly ImmutableList<Value> _values;

    private Env(ImmutableList<Value> values) => _values = values;

    public static readonly Env Empty = new(ImmutableList<Value>.Empty);

    /// <summary>This scope with <paramref name="value"/> as its innermost entry.</summary>
    public Env Push(Value value) => new(_values.Insert(0, value));

    /// <summary>The entry a de Bruijn index names.</summary>
    public Value this[int index] => _values[index];

    public int Count => _values.Count;
}
