using Fun.Kernel;

namespace Fun.Compiler;

/// <summary>
/// E11's generative half: a module whose evaluation performs something is
/// generative - each evaluation is a new type - and the binder of such a value
/// names it. In the binder's type every nominal the module declares becomes that
/// member of the binder, so <c>m1 = mk(())</c> gives <c>m1.make : I64 -&gt; m1.T</c>,
/// shared with no other evaluation; a sealed type may not leave the binder's scope.
/// </summary>
// ponytail: no run-time stamp yet (the prototype's first private module slot), so two
// evaluations of a generative module would compare equal in a type-case: a type-case
// head on a generative nominal raises "not ported yet" instead (Elaborator.Patterns).
public static partial class Elaborator
{
    /// <summary>
    /// A module's bindings in a sink of their own, so the module knows whether its
    /// evaluation performs; what they performed then reaches the enclosing form. Every
    /// nominal declared meanwhile becomes generative when it did - labelled by the
    /// member it is bound to, where it is bound directly.
    /// </summary>
    private static (Term, Value) Generative(Context ctx, Func<Context, (Term Term, Value Type, IReadOnlyList<BindingTerm> Bindings)> elaborate)
    {
        var firstDeclared = ctx.Metas.DeclaredNominals.Count;
        var ((term, type, bindings), performed) = Collecting(ctx, c => elaborate(c));
        Emit(ctx, performed);
        if (!performed.IsEmpty)
        {
            var labels = bindings.OfType<BindingTerm.Let>()
                .Where(l => l.Def is Term.Nominal)
                .ToDictionary(l => ((Term.Nominal)l.Def).Decl, l => l.Name);
            foreach (var decl in ctx.Metas.DeclaredNominals.Skip(firstDeclared))
                ctx.Metas.GenerativeNominals[decl] = labels.GetValueOrDefault(decl);
        }
        return (term, type);
    }

    /// <summary>
    /// <paramref name="type"/> as the type of a binder pushed at <paramref name="ctx"/>'s
    /// width whose value performs: each generative nominal it mentions becomes that
    /// member of the binder.
    /// </summary>
    private static Value Seal(Context ctx, Value type)
    {
        var width = ctx.Width + 1;
        var quoted = Nbe.Quote(ctx.Metas, width, type);
        if (!Mentions(ctx.Metas, quoted)) return type;

        var sealedTerm = quoted.Map((t, under) => t is Term.Nominal n && ctx.Metas.GenerativeNominals.TryGetValue(n.Decl, out var label)
            ? new Term.Dot(new Term.Var(under), label
                ?? throw new NotImplementedException("not ported yet: sealing a generative nominal that is not bound as a module member"))
            : null);
        return Nbe.Eval(ctx.Metas, ctx.Environment.Push(new Value.VVar(ctx.Width, [])), sealedTerm);
    }

    private static bool Mentions(MetaContext mc, Term term)
    {
        var found = false;
        term.Map((t, _) =>
        {
            if (t is Term.Nominal n && mc.GenerativeNominals.ContainsKey(n.Decl)) found = true;
            return found ? t : null;
        });
        return found;
    }

    /// <summary>
    /// A sealed binder's types may not leave its scope: <paramref name="type"/>, read at
    /// <paramref name="width"/>, names no entry at a level from <paramref name="inner"/> up.
    /// </summary>
    private static void CheckSealedStays(Context ctx, int inner, int width, string name, Value type)
    {
        var escapes = false;
        Nbe.Quote(ctx.Metas, width, type).Map((t, under) =>
        {
            if (t is Term.Var v && v.Index >= under && width - 1 - (v.Index - under) >= inner) escapes = true;
            return escapes ? t : null;
        });
        if (escapes) throw new FunException($"a generative type escapes its binder: {Label(name)}");
    }

    /// <summary>A member of a generative module no binder names may not have a type mentioning a type the module declares.</summary>
    private static void CheckGenerativeEscape(Context ctx, EffectSink headPerformed, string member, Value memberType)
    {
        if (!headPerformed.IsEmpty && Mentions(ctx.Metas, ctx.Quote(memberType)))
            throw new FunException($"a generative type escapes an unnamed module: {member}");
    }
}
