using Fun.Compiler;
using Fun.Kernel;

namespace Fun.Tests;

/// <summary>
/// A nominal's identity is its declaration plus the values of its captures,
/// compared by conversion (E11).
/// </summary>
public class NominalTests
{
    private static (MetaContext Metas, EquatableArray<Value> Items) Tuple(string source)
    {
        var program = Driver.Elaborate(source, new Dictionary<string, string>());
        var value = Assert.IsType<Value.VProd>(Driver.Run(program));
        return (program.Context.Metas, value.Items);
    }

    [Fact]
    public void OneDeclarationOverConvertibleCapturesIsOneType()
    {
        var (mc, types) = Tuple("{ F = fn(A : Type) { enum { X(A) } }; (F(I64), F(I64), F(Char)) }");

        Unify.Values(mc, 0, types[0], types[1]);
        Assert.Throws<UnifyException>(() => Unify.Values(mc, 0, types[0], types[2]));
    }

    [Fact]
    public void TwoDeclarationsAreTwoTypes()
    {
        var (mc, types) = Tuple("(enum { A }, enum { A })");

        Assert.Throws<UnifyException>(() => Unify.Values(mc, 0, types[0], types[1]));
    }

    /// <summary>A parameter nothing in the declaring function body names does not split the type.</summary>
    [Fact]
    public void AnUnmentionedParameterIsNotCaptured()
    {
        var (mc, types) = Tuple("{ F = fn(n : I64) { enum { X } }; (F(0), F(1)) }");

        Assert.Empty(Assert.IsType<Value.VNominal>(types[0]).Captures);
        Unify.Values(mc, 0, types[0], types[1]);
    }
}
