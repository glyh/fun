using Fun.Compiler;
using Fun.Kernel;

namespace Fun.Tests;

public class UnifyTests
{
    private static Value Pi(Term domain, Term codomain) =>
        Nbe.Eval(new MetaContext(), Environment.Empty, new Term.Pi(Explicitness.Explicit, domain, codomain));

    /// <summary>
    /// <c>?0[x] = (T : Type) -&gt; T -&gt; T</c>: the solution mentions its own
    /// binder, so solving has to lift the renaming under it. The prototype raises
    /// VarNotInSpine here (meta-solution-renaming-not-lifted-under-binders).
    /// </summary>
    [Fact]
    public void SolvesADependentRightHandSideUnderASpine()
    {
        var mc = new MetaContext();
        var meta = mc.Fresh();
        var x = new Value.VVar(5, []);
        var dependent = new Term.Pi(Explicitness.Explicit, Term.U.Instance,
            new Term.Pi(Explicitness.Explicit, new Term.Var(0), new Term.Var(1)));

        Unify.Values(mc, 6, new Value.VMeta(meta, [x]), Nbe.Eval(mc, Environment.Empty, dependent));

        // The solution is `fn(_) { (T : Type) -> T -> T }`: applied to x, it reads back as the type.
        var solved = Nbe.Apply(mc, mc.Solution(meta)!, x);
        Assert.Equal(dependent, Nbe.Quote(mc, 6, solved));
    }

    /// <summary>A spine variable becomes the solution's own parameter.</summary>
    [Fact]
    public void AbstractsOverTheSpine()
    {
        var mc = new MetaContext();
        var meta = mc.Fresh();

        // ?0[#2, #4] = (#4, #2)
        Unify.Values(mc, 5,
            new Value.VMeta(meta, [new Value.VVar(2, []), new Value.VVar(4, [])]),
            new Value.VProdTy([new Value.VVar(4, []), new Value.VVar(2, [])]));

        Assert.Equal(new Term.Lam(new Term.Lam(new Term.ProdTy([new Term.Var(0), new Term.Var(1)]))),
            Nbe.Quote(mc, 0, mc.Solution(meta)!));
    }

    [Fact]
    public void RejectsAVariableTheSpineDoesNotAbstract()
    {
        var mc = new MetaContext();
        var meta = mc.Fresh();

        Assert.Throws<UnifyException>(() =>
            Unify.Values(mc, 5, new Value.VMeta(meta, [new Value.VVar(2, [])]), new Value.VVar(3, [])));
    }

    [Fact]
    public void RejectsANonLinearSpine()
    {
        var mc = new MetaContext();
        var meta = mc.Fresh();
        var x = new Value.VVar(2, []);

        Assert.Throws<UnifyException>(() =>
            Unify.Values(mc, 5, new Value.VMeta(meta, [x, x]), new Value.VAtomTy(AtomTy.I64)));
    }

    private static ModuleEntry Member(string name, MemberKind kind, AtomTy type) =>
        new ModuleEntry.Field(name, kind, new Value.VAtomTy(type));

    /// <summary>
    /// A partial module type (a signature's instance) needs only its own public
    /// members in the other side; a private member never counts.
    /// </summary>
    [Fact]
    public void PartialModuleTypesAreWidthSubtyped()
    {
        var mc = new MetaContext();
        var wanted = new Value.VModule([Member("x", MemberKind.Public, AtomTy.I64)], Partial: true);

        Unify.Values(mc, 0, wanted, new Value.VModule(
            [Member("x", MemberKind.Public, AtomTy.I64), Member("y", MemberKind.Public, AtomTy.Char)], Partial: false));
        Assert.Throws<UnifyException>(() => Unify.Values(mc, 0, wanted,
            new Value.VModule([Member("x", MemberKind.Private, AtomTy.I64)], Partial: false)));
        Assert.Throws<UnifyException>(() => Unify.Values(mc, 0, wanted,
            new Value.VModule([Member("x", MemberKind.Public, AtomTy.Char)], Partial: false)));
    }

    /// <summary>A field of unknown-typed value asks for any struct holding it: a partial struct.</summary>
    [Fact]
    public void PartialStructTypesAreWidthSubtyped()
    {
        var mc = new MetaContext();
        var point = new Value.VStruct(
            [Member("x", MemberKind.Field, AtomTy.I64), Member("y", MemberKind.Field, AtomTy.I64)], Partial: false);

        Unify.Values(mc, 0, new Value.VStruct([Member("y", MemberKind.Field, AtomTy.I64)], Partial: true), point);
        Assert.Throws<UnifyException>(() => Unify.Values(mc, 0,
            new Value.VStruct([Member("x", MemberKind.Field, AtomTy.I64)], Partial: false), point));
    }
}
