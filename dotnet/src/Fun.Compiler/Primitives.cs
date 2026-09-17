using Fun.Kernel;

namespace Fun.Compiler;

/// <summary>
/// One primitive: its name, its type, and how it reduces. <paramref name="Reduce"/>
/// receives the frames applied so far and returns the result, or null while the
/// application is stuck (too few arguments, or one is not yet known); a failure is
/// a <see cref="FunException"/>. A null <paramref name="Type"/> marks a primitive no
/// program names: it is reached only through another primitive's type.
/// </summary>
public sealed record PrimitiveDeclaration(
    string Name, Value? Type, Func<MetaContext, EquatableArray<Frame>, Value?> Reduce)
{
    /// <summary>A primitive that reduces on its arguments alone.</summary>
    public PrimitiveDeclaration(string name, Value? type, Func<EquatableArray<Frame>, Value?> reduce)
        : this(name, type, (_, frames) => reduce(frames)) { }
}

/// <summary>
/// The one declaration of every primitive (unify-primitive-declaration.md). The base
/// context binds each named one as a defined entry; the evaluator reduces through
/// <see cref="Reduce"/>. Nothing else lists primitives.
/// </summary>
public static class Primitives
{
    private const string Tuple = "Tuple";
    private const string TupleArity = "tuple_arity";

    /// <summary>A pure arrow <c>domain -&gt; codomain</c>, the codomain a closed term.</summary>
    private static Value Arrow(Value domain, Term codomain) =>
        new Value.VPi(Explicitness.Explicit, domain, new Closure(Environment.Empty, codomain));

    /// <summary><c>[A : Type] -&gt; body</c>, the body over the implicit binder.</summary>
    private static Value OverType(Term body) =>
        new Value.VPi(Explicitness.Implicit, Value.VU.Instance, new Closure(Environment.Empty, body));

    private static Value Binary(AtomTy operand, AtomTy result) =>
        Arrow(new Value.VAtomTy(operand),
            new Term.Pi(Explicitness.Explicit, new Term.AtomTy(operand), new Term.AtomTy(result)));

    public static readonly IReadOnlyList<PrimitiveDeclaration> Declarations =
    [
        new("+", Binary(AtomTy.I64, AtomTy.I64), I64Arith("+", CheckedAdd)),
        new("-", Binary(AtomTy.I64, AtomTy.I64), I64Arith("-", CheckedSub)),
        new("*", Binary(AtomTy.I64, AtomTy.I64), I64Arith("*", CheckedMul)),
        new("/", Binary(AtomTy.I64, AtomTy.I64), I64Div("/")),
        new("%", Binary(AtomTy.I64, AtomTy.I64), I64Div("%")),
        new("eq_i64", Binary(AtomTy.I64, AtomTy.I64), I64Cmp((a, b) => a == b)),
        new("neq_i64", Binary(AtomTy.I64, AtomTy.I64), I64Cmp((a, b) => a != b)),
        new("lt_i64", Binary(AtomTy.I64, AtomTy.I64), I64Cmp((a, b) => a < b)),
        new("gt_i64", Binary(AtomTy.I64, AtomTy.I64), I64Cmp((a, b) => a > b)),
        new("le_i64", Binary(AtomTy.I64, AtomTy.I64), I64Cmp((a, b) => a <= b)),
        new("ge_i64", Binary(AtomTy.I64, AtomTy.I64), I64Cmp((a, b) => a >= b)),
        new("eq_char", Binary(AtomTy.Char, AtomTy.I64), Compare((Atom.Char a, Atom.Char b) => a.Value == b.Value)),
        new("neq_char", Binary(AtomTy.Char, AtomTy.I64), Compare((Atom.Char a, Atom.Char b) => a.Value != b.Value)),
        new("eq_unit", Binary(AtomTy.Unit, AtomTy.I64), Compare((Atom.Unit _, Atom.Unit _) => true)),
        new("neq_unit", Binary(AtomTy.Unit, AtomTy.I64), Compare((Atom.Unit _, Atom.Unit _) => false)),
        new("eq_string", Binary(AtomTy.String, AtomTy.I64), Compare((Atom.Str a, Atom.Str b) => a.Value == b.Value)),
        new("neq_string", Binary(AtomTy.String, AtomTy.I64), Compare((Atom.Str a, Atom.Str b) => a.Value != b.Value)),

        // panic[A](message) : A
        new("panic", OverType(new Term.Pi(Explicitness.Explicit, new Term.AtomTy(AtomTy.String), new Term.Var(1))), Panic),

        // expand_block[Syntax.Expr](b), expand_decls[Syntax.Decls](d): typed in the
        // prelude's Syntax module; they run only inside a macro application.
        new("expand_block", OverType(new Term.Pi(Explicitness.Explicit, new Term.Var(0), new Term.Var(1))), MacroRuntime("expand_block", a => a.ExpandBlock)),
        new("expand_decls", OverType(new Term.Pi(Explicitness.Explicit, new Term.Var(0), new Term.Var(1))), MacroRuntime("expand_decls", a => a.ExpandDecls)),

        // Tuple : (n : I64) -> tuple_arity(n); Tuple(n, T1, ..., Tn) is the flat product.
        new(Tuple, Arrow(new Value.VAtomTy(AtomTy.I64),
            new Term.Ap(new Term.Prim(TupleArity), Explicitness.Explicit, new Term.Var(0))), TupleType),
        new(TupleArity, null, TupleArityType),
    ];

    private static readonly Dictionary<string, PrimitiveDeclaration> ByName =
        Declarations.ToDictionary(d => d.Name);

    /// <summary>The application of primitive <paramref name="name"/> to <paramref name="frames"/>, or null while stuck.</summary>
    public static Value? Reduce(MetaContext mc, string name, EquatableArray<Frame> frames) =>
        ByName.TryGetValue(name, out var declaration) ? declaration.Reduce(mc, frames) : null;

    // ---- reducers -----------------------------------------------------------

    /// <summary>Reduces once exactly two arguments are both atoms; null keeps it stuck.</summary>
    private static Func<EquatableArray<Frame>, Value?> Atoms2(Func<Atom, Atom, Atom?> reduce) => frames =>
        frames.Length == 2
            && frames[0] is Frame.FApp { Arg: Value.VAtom a }
            && frames[1] is Frame.FApp { Arg: Value.VAtom b }
            && reduce(a.Atom, b.Atom) is { } result
            ? new Value.VAtom(result)
            : null;

    private static Atom I64(long n) => new Atom.I64(n);

    // Predicates return I64 1/0: Bool is a library enum, never an atom.
    private static Atom Truth(bool b) => I64(b ? 1 : 0);

    private static Func<EquatableArray<Frame>, Value?> I64Cmp(Func<long, long, bool> f) =>
        Atoms2((a, b) => a is Atom.I64 x && b is Atom.I64 y ? Truth(f(x.Value, y.Value)) : null);

    private static Func<EquatableArray<Frame>, Value?> Compare<TA>(Func<TA, TA, bool> f) where TA : Atom =>
        Atoms2((a, b) => a is TA x && b is TA y ? Truth(f(x, y)) : null);

    /// <summary>I64 arithmetic is checked: overflow is a language error, never the host's wrap-around.</summary>
    private static Func<EquatableArray<Frame>, Value?> I64Arith(string op, Func<long, long, long?> f) =>
        Atoms2((a, b) => a is Atom.I64 x && b is Atom.I64 y
            ? I64(f(x.Value, y.Value) ?? throw new FunException($"integer overflow in {op}"))
            : null);

    private static long? CheckedAdd(long a, long b)
    {
        var r = unchecked(a + b);
        return ((a ^ r) & (b ^ r)) < 0 ? null : r;
    }

    private static long? CheckedSub(long a, long b)
    {
        var r = unchecked(a - b);
        return ((a ^ b) & (a ^ r)) < 0 ? null : r;
    }

    private static long? CheckedMul(long a, long b)
    {
        var high = Math.BigMul(a, b, out var low);
        return high == low >> 63 ? low : null;
    }

    /// <summary>
    /// Division by zero is an error, and so is <c>MinValue / -1</c>; <c>MinValue % -1</c>
    /// is 0, where the host would throw.
    /// </summary>
    private static Func<EquatableArray<Frame>, Value?> I64Div(string op) =>
        Atoms2((a, b) =>
        {
            if (a is not Atom.I64 x || b is not Atom.I64 y) return null;
            if (y.Value == 0) throw new FunException("division by zero");
            if (y.Value == -1)
            {
                if (op == "%") return I64(0);
                if (x.Value == long.MinValue) throw new FunException($"integer overflow in {op}");
                return I64(-x.Value);
            }
            return I64(op == "/" ? x.Value / y.Value : x.Value % y.Value);
        });

    /// <summary><c>panic[A](message)</c> fails with its message once the message is known.</summary>
    private static Value? Panic(EquatableArray<Frame> frames) =>
        frames.Length == 2 && frames[1] is Frame.FApp { Arg: Value.VAtom { Atom: Atom.Str message } }
            ? throw new FunException(message.Value)
            : null;

    /// <summary>Asks the running macro application to expand its argument; stuck on an unknown one.</summary>
    private static Func<MetaContext, EquatableArray<Frame>, Value?> MacroRuntime(string name, Func<MacroApplication, Func<Value, Value>> expand) =>
        (mc, frames) => frames.Length == 2 && frames[1] is Frame.FApp { Arg: not (Value.VVar or Value.VMeta or Value.VNeutral) } arg
            ? mc.Budget.Application is { } application
                ? expand(application)(arg.Arg)
                : throw new FunException($"`{name}` runs only inside a macro application")
            : null;

    /// <summary><c>tuple_arity(n)</c>: <c>Type</c> after no more arguments, else <c>Type -&gt; tuple_arity(n - 1)</c>.</summary>
    private static Value? TupleArityType(EquatableArray<Frame> frames)
    {
        if (frames.Length != 1 || frames[0] is not Frame.FApp { Arg: Value.VAtom { Atom: Atom.I64 n } }) return null;
        if (n.Value < 0) throw new FunException("Tuple: the number of components is negative");
        if (n.Value == 0) return Value.VU.Instance;
        return new Value.VPi(Explicitness.Explicit, Value.VU.Instance,
            new Closure(Environment.Empty.Push(new Value.VAtom(I64(n.Value - 1))),
                new Term.Ap(new Term.Prim(TupleArity), Explicitness.Explicit, new Term.Var(1))));
    }

    /// <summary><c>Tuple(n, T1, …, Tn)</c> once all <c>n</c> component types are applied.</summary>
    private static Value? TupleType(EquatableArray<Frame> frames)
    {
        if (frames.Length == 0 || frames[0] is not Frame.FApp { Arg: Value.VAtom { Atom: Atom.I64 n } }) return null;
        var components = frames.Skip(1).ToList();
        if (components.Count != n.Value || components.Any(f => f is not Frame.FApp)) return null;
        return new Value.VProdTy([.. components.Select(f => ((Frame.FApp)f).Arg)]);
    }
}
