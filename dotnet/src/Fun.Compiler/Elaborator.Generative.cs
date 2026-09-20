using System.Collections.Immutable;
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
public sealed partial record Context
{
    /// <summary>
    /// The nominals a sealed binder names, by binder level and member label (E11). A
    /// type-case head like <c>st1.Symbol</c> is a projection on a sealed binder, so its
    /// declaration is read here.
    /// </summary>
    public ImmutableDictionary<int, ImmutableDictionary<string, NominalDecl>> Sealed { get; init; } =
        ImmutableDictionary<int, ImmutableDictionary<string, NominalDecl>>.Empty;
}

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
    /// member of the binder, and is reported by the label it became. An empty report
    /// means the type was not sealed - it mentions no generative nominal - and the
    /// type is returned unchanged.
    /// </summary>
    private static (Value Type, ImmutableDictionary<string, NominalDecl> Sealed) Seal(Context ctx, Value type)
    {
        var width = ctx.Width + 1;
        var quoted = Nbe.Quote(ctx.Metas, width, type);
        if (!Mentions(ctx.Metas, quoted)) return (type, ImmutableDictionary<string, NominalDecl>.Empty);

        var sealedNominals = ImmutableDictionary<string, NominalDecl>.Empty;
        var sealedTerm = quoted.Map((t, under) =>
        {
            if (t is not Term.Nominal n || !ctx.Metas.GenerativeNominals.TryGetValue(n.Decl, out var label)) return null;
            if (label is null)
                throw new NotImplementedException("not ported yet: sealing a generative nominal that is not bound as a module member");
            sealedNominals = sealedNominals.SetItem(label, n.Decl);
            return new Term.Dot(new Term.Var(under), label);
        });
        return (Nbe.Eval(ctx.Metas, ctx.Environment.Push(new Value.VVar(ctx.Width, [])), sealedTerm), sealedNominals);
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
    /// The binder a performing value gets: a rigid entry whose type is sealed, and the
    /// sealing context that lets a type-case head name the declaration behind it (E11).
    /// </summary>
    private static (Context Body, ImmutableDictionary<string, NominalDecl> Sealed) SealBinder(Context ctx, string name, Value type)
    {
        var (sealedType, sealedNominals) = Seal(ctx, type);
        var body = ctx.Bind(name, sealedType);
        return (sealedNominals.IsEmpty ? body : body with { Sealed = body.Sealed.SetItem(ctx.Width, sealedNominals) }, sealedNominals);
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
