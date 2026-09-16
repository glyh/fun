using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Nbe
{
    private abstract partial record Kont
    {
        /// <summary>
        /// The function is evaluated; apply it to <paramref name="Arg"/>. A fixpoint
        /// unfold pushes this for its member's body, having already been
        /// <paramref name="Charged"/> for the call.
        /// </summary>
        public sealed record ApplyArg(Value Arg, bool Charged) : Kont;
    }

    /// <summary>
    /// Applies <paramref name="fn"/> to <paramref name="arg"/> inside the machine.
    /// Returns what to evaluate next -- a closure's body, or a fixpoint member's
    /// body with the application pending -- or null with <paramref name="result"/>
    /// set when the application is already a value.
    /// </summary>
    private static (Environment, Term)? Enter(
        MetaContext mc, Stack<Kont> stack, Value fn, Value arg, bool charged, out Value? result)
    {
        result = null;
        switch (fn)
        {
            case Value.VLam lam:
                if (!charged) mc.Budget.Spend("a function");
                return (lam.Body.Environment.Push(arg), lam.Body.Body);

            // While checking, a call of a known-pure fixpoint is deferred: conversion
            // compares two such calls by their arguments before unfolding either.
            case Value.VFix fix when fix.Member.Pure && mc.Budget.Checking:
                result = new Value.VGlued(fix, arg, new Lazy<Value>(() => UnfoldCall(mc, fix, arg)));
                return null;

            case Value.VFix fix:
                return Unfold(mc, stack, fix, arg);

            case Value.VCont cont:
                result = Resume(stack, cont, arg);
                return null;

            case Value.VGlued glued:
                // The deferred call is the function: unfold it, then apply its result.
                stack.Push(new Kont.ApplyArg(arg, Charged: false));
                return Unfold(mc, stack, glued.Fix, glued.Arg);

            default:
                result = ApplyStuck(mc, fn, arg);
                return null;
        }
    }

    /// <summary>A fixpoint call: one charge, then the member's body applied to the argument.</summary>
    private static (Environment, Term) Unfold(MetaContext mc, Stack<Kont> stack, Value.VFix fix, Value arg)
    {
        mc.Budget.Spend(fix.Member.Name, isFixpoint: true);
        stack.Push(new Kont.ApplyArg(arg, Charged: true));
        return (fix.BodyEnvironment(), fix.Member.Body);
    }

    /// <summary>A deferred call, unfolded when something first inspects it.</summary>
    private static Value UnfoldCall(MetaContext mc, Value.VFix fix, Value arg) =>
        mc.Budget.Request("an evaluation", () =>
        {
            var stack = new Stack<Kont>();
            var (env, term) = Unfold(mc, stack, fix, arg);
            return Machine(mc, env, term, stack);
        });

    /// <summary>
    /// Whether a frame needs the shape of the value it receives, so a deferred call
    /// handed to it is unfolded first -- in the loop, never by recursing.
    /// </summary>
    private static bool NeedsShape(Kont waiting) =>
        waiting is Kont.ApplyArg or Kont.ProjOf or Kont.DotOf or Kont.OpenBody or Kont.ModuleOpen;

    /// <summary>
    /// A recursive group's member read back: every body evaluated under a variable
    /// per member, the first outermost, then quoted that many entries further in.
    /// </summary>
    private static Term QuoteFix(MetaContext mc, int width, Value.VFix fix)
    {
        var count = fix.Members.Length;
        var env = Enumerable.Range(0, count).Aggregate(fix.Environment, (e, i) => e.Push(new Value.VVar(width + i, [])));
        return new Term.Fix(
            [.. fix.Members.Select(m => m with { Body = Quote(mc, width + count, Eval(mc, env, m.Body)) })],
            fix.Index);
    }

    /// <summary>
    /// Whether two values are convertible without solving any meta: here, whether
    /// they read back to the same term. Deferred calls read back as calls.
    /// </summary>
    // ponytail: read-back equality has no eta and does not unfold, so it can say
    // "not convertible" where full conversion would not; its only caller, the
    // lazy-delta shortcut, then falls back to unfolding, which is still correct.
    public static bool Convertible(MetaContext mc, int width, Value left, Value right) =>
        Quote(mc, width, left) == Quote(mc, width, right);
}
