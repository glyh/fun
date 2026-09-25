using Fun.Compiler;
using Fun.Kernel;

namespace Fun.Tests;

public class EffectTests
{
    private static readonly EffectFamily Ask = new(9001, "Ask", 0, [new EffectOperation("op", Term.U.Instance, Term.U.Instance)]);

    /// <summary>
    /// A request raised a million frames deep is handled and resumed: raising pops
    /// those frames into the continuation and resuming pushes them back, on the
    /// machine's heap stack, never the CLR's.
    /// </summary>
    [Fact]
    public void PerformAndResumeDeepInsideAHandlerDoNotUseTheNativeStack()
    {
        // let Ask = effect in match (id(id(…(perform Ask.op(0))…))) { x => x, effect Ask.op _ => resume(42) }
        Term body = new Term.Perform(new Term.Var(0), "op", new Term.Atom(new Atom.I64(0)));
        for (var i = 0; i < 1_000_000; i++)
            body = new Term.Ap(new Term.Lam(new Term.Var(0)), Explicitness.Explicit, body);
        var resume = new Term.Ap(new Term.Var(0), Explicitness.Explicit, new Term.Atom(new Atom.I64(42)));
        var handler = new Term.Match(body, [new Term.Var(0)], new DecisionTree.Leaf(0, [Occurrence.Base.Instance]))
        {
            EffectBranches = [new EffectBranchTerm(new Term.Var(0), "op", new DecisionTree.Leaf(0, []), resume)],
            Handler = 1,
        };
        var term = new Term.Let(Term.U.Instance, new Term.EffectDecl(Ask), handler);

        Assert.Equal(new Value.VAtom(new Atom.I64(42)), Nbe.Eval(new MetaContext(), Environment.Empty, term));
    }

    /// <summary>A continuation resumes once (E7).</summary>
    [Fact]
    public void AContinuationIsOneShot()
    {
        // match (perform Ask.op(0)) { x => x, effect Ask.op _ => { _ = resume(1); resume(2) } }
        var twice = new Term.Let(Term.U.Instance,
            new Term.Ap(new Term.Var(0), Explicitness.Explicit, new Term.Atom(new Atom.I64(1))),
            new Term.Ap(new Term.Var(1), Explicitness.Explicit, new Term.Atom(new Atom.I64(2))));
        var handler = new Term.Match(new Term.Perform(new Term.Var(0), "op", new Term.Atom(new Atom.I64(0))),
            [new Term.Var(0)], new DecisionTree.Leaf(0, [Occurrence.Base.Instance]))
        {
            EffectBranches = [new EffectBranchTerm(new Term.Var(0), "op", new DecisionTree.Leaf(0, []), twice)],
            Handler = 1,
        };
        var term = new Term.Let(Term.U.Instance, new Term.EffectDecl(Ask), handler);

        var error = Assert.Throws<FunException>(() => new MetaContext().Budget.Run(() => Nbe.Eval(new MetaContext(), Environment.Empty, term)));
        Assert.Equal("continuation already used", error.Message);
    }

    /// <summary>
    /// An unhandled effect the checker's own evaluation performs is a language error naming
    /// the effect and the form being inferred (the prototype's EvaluationFailed shape), not
    /// an unported path.
    /// </summary>
    [Fact]
    public void AnUnhandledEffectTheCheckerEvaluatesNamesTheFormBeingInferred()
    {
        const string source = """
            { effect Abort = sig { stop : Unit -> I64 };
              f = fn(u : Unit) { perform Abort.stop(u); I64 };
              E = enum { C(f(())) };
              1 }
            """;
        var message = Assert.Throws<FunException>(() => Driver.Elaborate(source, new Dictionary<string, string>())).Message;
        Assert.StartsWith("unhandled effect Abort.stop: no handler for it is in scope while inferring the form at", message);
        Assert.EndsWith("(while type checking)", message);
    }
}
