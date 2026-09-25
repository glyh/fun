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

    /// <summary>
    /// A former's parameter nothing in its body names is refused at the declaration,
    /// naming the parameter (ruling 2026-09-25) - not at sealing, whose message and
    /// site are different.
    /// </summary>
    [Fact]
    public void AnUnmentionedParameterIsRefusedAtItsDeclaration()
    {
        var ex = Assert.Throws<FunException>(() => Tuple("{ F = fn(n : I64) { enum { X } }; (F(0), F(1)) }"));
        Assert.Contains("does not occur in its body", ex.Message);
        Assert.Contains("'n#", ex.Message);
    }

    // ---- the module stamp (E11) ------------------------------------------------

    private static Elaborated Module(string body) =>
        Driver.Elaborate($"module {{ {body} }}", new Dictionary<string, string>());

    /// <summary>Every module's first slot is a private stamp, so both sides push the same width (I2).</summary>
    [Fact]
    public void AModulesFirstSlotIsItsStamp()
    {
        var program = Module("pub x = 1");

        var stamp = Assert.IsType<BindingTerm.Let>(Assert.IsType<Term.Module>(Unopen(program.Term)).Bindings[0]);
        Assert.Equal("#stamp", stamp.Name);
        Assert.Equal(MemberKind.Private, stamp.Kind);
        Assert.Equal(new Term.Atom(Atom.Unit.Instance), stamp.Def);
        Assert.Equal(1, (stamp.Slots() ?? throw new InvalidOperationException("a let always has a slot")).Length);

        var field = Assert.IsType<ModuleEntry.Field>(Assert.IsType<Value.VModule>(program.Type).Entries[0]);
        Assert.Equal("#stamp", field.Name);
        Assert.Equal(MemberKind.Private, field.Kind);
        Assert.Equal(new Value.VAtomTy(AtomTy.Unit), field.Value);
    }

    /// <summary>A pure module's stamp is <c>()</c>: its evaluations share one type.</summary>
    [Fact]
    public void APureModulesStampIsUnit()
    {
        var module = Assert.IsType<Value.VModule>(Driver.Run(Module("pub x = 1")));
        Assert.Equal(new Value.VAtom(Atom.Unit.Instance), Assert.IsType<ModuleEntry.Field>(module.Entries[0]).Value);
    }

    /// <summary>A module whose evaluation performs mints a fresh stamp cell each time.</summary>
    [Fact]
    public void AGenerativeModulesStampIsAFreshCell()
    {
        var program = Module("t = ref(0); pub get = fn(u : Unit) { deref(t) }");

        var first = Stamp(Driver.Run(program));
        var second = Stamp(Driver.Run(program));
        Assert.IsType<Value.VRef>(first);
        Assert.NotSame(((Value.VRef)first).Cell, ((Value.VRef)second).Cell);

        static Value Stamp(Value value) => Assert.IsType<ModuleEntry.Field>(Assert.IsType<Value.VModule>(value).Entries[0]).Value;
    }

    /// <summary>A program elaborates inside <c>open (import "std")</c>, so its own term is behind one.</summary>
    private static Term Unopen(Term term) => term is Term.Open open ? Unopen(open.Body) : term;

    /// <summary>Every nominal a module declares captures its stamp, one slot shared by all.</summary>
    [Fact]
    public void AModulesNominalsCaptureItsStamp()
    {
        var module = Assert.IsType<Value.VModule>(Driver.Run(Module("pub type T = A | B")));
        var nominal = Assert.IsType<Value.VNominal>(module.Entries.OfType<ModuleEntry.Field>().Last(e => e.Name == "T").Value);
        Assert.Contains(nominal.Captures, c => c is Value.VAtom { Atom: Atom.Unit });
    }
}
