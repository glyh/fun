namespace Fun.Kernel;

/// <summary>
/// An effect row as a term: the effects a function may perform when called, and
/// the row variables (tails) whose effects it also has (E2). The empty row is pure.
/// </summary>
public sealed record RowTerm(EquatableArray<Term> Effects, EquatableArray<Term> Tails)
{
    public static readonly RowTerm Pure = new([], []);

    public bool IsPure => Effects.IsEmpty && Tails.IsEmpty;
}

/// <summary>An arrow's row closed over the environment its arrow was written in; read under the arrow's binder.</summary>
public sealed record RowClosure(Environment Environment, RowTerm Row)
{
    public static readonly RowClosure Pure = new(Environment.Empty, RowTerm.Pure);
}

/// <summary>An effect operation: its input and output types, read under the family's parameters.</summary>
public sealed record EffectOperation(string Name, Term Input, Term Output);

/// <summary>
/// An effect family's declaration. Its identity is <paramref name="Id"/> and the
/// values of its parameters (E1); <paramref name="Name"/> is for display.
/// </summary>
public sealed record EffectFamily(int Id, string Name, int ParamCount, EquatableArray<EffectOperation> Operations);

public abstract partial record Term
{
    /// <summary>The type of effect rows.</summary>
    public sealed record EffectRowTy : Term
    {
        public static readonly EffectRowTy Instance = new();
    }

    public sealed record EffectRowLit(RowTerm Row) : Term;

    /// <summary>
    /// An effect family declared in a block: pushes the family, its operations
    /// closed over the environment here, then runs the body.
    /// </summary>
    public sealed record EffectDef(EffectFamily Family, Term Body) : Term;

    /// <summary>
    /// An effect instance: <paramref name="Family"/> closed over
    /// <paramref name="Environment"/> (as the declaration left it), applied to
    /// <paramref name="Params"/>. Readback's form for an effect value.
    /// </summary>
    public sealed record Effect(EffectFamily Family, Environment Environment, EquatableArray<Term> Params) : Term;

    /// <summary><c>perform</c> of an operation of an effect instance.</summary>
    public sealed record Perform(Term Instance, string Op, Term Arg) : Term;

    /// <summary>
    /// A call whose row has an open tail (tunneling, E5): a request coming out of
    /// it whose effect instance is none of <paramref name="Named"/> belongs to the
    /// caller's caller, so it skips <paramref name="Handlers"/> - the handlers
    /// lexically enclosing the call in its function body.
    /// </summary>
    public sealed record Tunnel(EquatableArray<Term> Named, EquatableArray<int> Handlers, Term Body) : Term;
}

/// <summary>
/// An effect branch of a handler: the instance and operation it handles, the
/// decision tree its argument pattern compiles to, and its body - read under the
/// pattern's binders, then the continuation innermost.
/// </summary>
public sealed record EffectBranchTerm(Term Instance, string Op, DecisionTree Argument, Term Body);

public abstract partial record Value
{
    public sealed record VEffectRowTy : Value
    {
        public static readonly VEffectRowTy Instance = new();
    }

    /// <summary>A normalised row: known effects and unsolved tails, a tail solved to a row spliced in.</summary>
    public sealed record VEffectRow(EquatableArray<Value> Effects, EquatableArray<Value> Tails) : Value
    {
        public static readonly VEffectRow Pure = new([], []);
    }

    /// <summary>
    /// An effect family instance: equal to another exactly when the family's id and
    /// the parameters agree (E1). The operations are closed over
    /// <paramref name="Environment"/>, the declaration's.
    /// </summary>
    public sealed record VEffect(EffectFamily Family, Environment Environment, EquatableArray<Value> Params) : Value;

    /// <summary>A one-shot continuation captured by a handler (E7).</summary>
    public sealed record VCont(Continuation Continuation) : Value;
}

/// <summary>
/// A captured continuation: a slice of the evaluator's frame stack. One-shot: a
/// second resume is a run-time error (E7).
/// </summary>
public abstract class Continuation
{
    public bool Used { get; set; }
}
