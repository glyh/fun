using Fun.Compiler;
using Fun.Kernel;

namespace Fun.Tests;

public class PrimitivesTests
{
    private static Value Prim(string name) =>
        Primitives.Declarations.Single(d => d.Name == name) is { Type: { } type }
            ? new Value.VNeutral(type, new Head.HPrim(name), [])
            : throw new InvalidOperationException(name);

    private static Value I64(long n) => new Value.VAtom(new Atom.I64(n));
    private static Value Str(string s) => new Value.VAtom(new Atom.Str(s));

    private static Value Apply(Value fn, params Value[] args)
    {
        var mc = new MetaContext();
        return args.Aggregate(fn, (f, a) => Nbe.Apply(mc, f, a));
    }

    [Theory]
    [InlineData("+", 2, 3, 5)]
    [InlineData("-", 2, 3, -1)]
    [InlineData("*", -4, 3, -12)]
    [InlineData("/", 7, -2, -3)]
    [InlineData("%", 7, -2, 1)]
    [InlineData("eq_i64", 4, 4, 1)]
    [InlineData("lt_i64", 5, 4, 0)]
    // MinValue % -1 is 0, where the host would throw.
    [InlineData("%", long.MinValue, -1, 0)]
    public void ReducesOnAtoms(string name, long a, long b, long expected) =>
        Assert.Equal(I64(expected), Apply(Prim(name), I64(a), I64(b)));

    [Theory]
    [InlineData("+", long.MaxValue, 1)]
    [InlineData("-", long.MinValue, 1)]
    [InlineData("*", 4611686018427387904, 2)]
    [InlineData("/", long.MinValue, -1)]
    public void CheckedArithmeticOverflowIsAnError(string name, long a, long b) =>
        Assert.Equal($"integer overflow in {name}",
            Assert.Throws<FunException>(() => Apply(Prim(name), I64(a), I64(b))).Message);

    [Fact]
    public void DivisionByZeroIsAnError() =>
        Assert.Equal("division by zero", Assert.Throws<FunException>(() => Apply(Prim("%"), I64(1), I64(0))).Message);

    /// <summary>An argument that is not an atom leaves the application stuck, headed by the primitive.</summary>
    [Fact]
    public void StaysStuckOnAnUnknownArgument()
    {
        var stuck = Assert.IsType<Value.VNeutral>(Apply(Prim("+"), new Value.VVar(0, []), I64(1)));
        Assert.Equal(new Head.HPrim("+"), stuck.Head);
        Assert.Equal(2, stuck.Frames.Length);
    }

    [Fact]
    public void StuckApplicationsUnifyByHeadAndArguments()
    {
        var x = new Value.VVar(0, []);
        Unify.Values(new MetaContext(), 1, Apply(Prim("eq_i64"), x, I64(1)), Apply(Prim("eq_i64"), x, I64(1)));
        Assert.Throws<UnifyException>(() =>
            Unify.Values(new MetaContext(), 1, Apply(Prim("eq_i64"), x, I64(1)), Apply(Prim("lt_i64"), x, I64(1))));
    }

    [Fact]
    public void PanicFailsWithItsMessageOnceKnown()
    {
        Assert.Equal("boom", Assert.Throws<FunException>(() => Apply(Prim("panic"), Value.VU.Instance, Str("boom"))).Message);
        Assert.IsType<Value.VNeutral>(Apply(Prim("panic"), Value.VU.Instance, new Value.VVar(0, [])));
    }

    /// <summary>expand_block and expand_decls answer the macro application they run in; outside one there is none.</summary>
    [Fact]
    public void MacroRuntimePrimitivesRunOnlyInsideAnApplication() =>
        Assert.Equal("`expand_block` runs only inside a macro application",
            Assert.Throws<FunException>(() => Apply(Prim("expand_block"), Value.VU.Instance, I64(0))).Message);

    /// <summary>
    /// The shared cases values/runtime-i64-overflow and runtime-division-by-zero
    /// elaborate and fail at evaluation, for exactly these reasons.
    /// </summary>
    [Theory]
    [InlineData("(+)(9223372036854775807, 1)", "integer overflow in +")]
    [InlineData("(/)(1, 0)", "division by zero")]
    public void ProgramsFailAtEvaluation(string source, string message)
    {
        var program = Driver.Elaborate(source, new Dictionary<string, string>());
        Assert.Equal(message, Assert.Throws<FunException>(() => Driver.Run(program)).Message);
    }

    [Fact]
    public void TupleIsTheFlatProductOfItsComponents() =>
        Assert.Equal(new Value.VProdTy([new Value.VAtomTy(AtomTy.I64), new Value.VAtomTy(AtomTy.Char)]),
            Apply(Prim("Tuple"), I64(2), new Value.VAtomTy(AtomTy.I64), new Value.VAtomTy(AtomTy.Char)));
}
