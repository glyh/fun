using System.Collections.Immutable;
using Fun.Kernel;

namespace Fun.Compiler;

/// <summary>
/// E11's generative half: a module whose evaluation performs something is
/// generative - each evaluation is a new type - and the binder of such a value
/// names it. In the binder's type every nominal the module declares becomes that
/// member of the binder, so <c>m1 = mk(())</c> gives <c>m1.make : I64 -&gt; m1.T</c>,
/// Every module also holds a private stamp its nominals capture, so a type-case
/// separates two evaluations of a generative module.
/// </summary>
// ponytail: a type-case head on a sealed generative nominal is still refused
// (Elaborator.Patterns): the declaration behind the projection is recorded in
// Context.Sealed, and reading it there is not ported yet.
public sealed partial record Context
{
    /// <summary>
    /// The levels a nominal declared in this scope always captures, beyond the names
    /// its enclosing module or body uses (E11): a module's stamp. Replaced whenever the
    /// enclosing module or body changes, as the prototype's <c>scope_captures</c> is.
    /// </summary>
    public ImmutableHashSet<int> ScopeCaptures { get; init; } = [];

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
    /// The module's stamp member (E11): private, and unwritable - <c>#</c> begins a
    /// resolved name no source spells.
    /// </summary>
    private const string ModuleStamp = "#stamp";

    private static readonly Value UnitType = new Value.VAtomTy(AtomTy.Unit);

    /// <summary>
    /// A module: its first slot is a private stamp every nominal it declares captures.
    /// The check-time stamp is <c>()</c>; a module whose evaluation performs gets a
    /// fresh cell at run time instead, so each evaluation's types are distinct
    /// instances (E11). Both sides read the slot list (I2), so the stamp moves no index
    /// by hand.
    /// </summary>
    private static (Term, Value) GenerativeModule(Context ctx, Syntax.Module module)
    {
        var firstDeclared = ctx.Metas.DeclaredNominals.Count;
        var outer = ctx.WithoutSelf() with { Enclosing = module };
        var stampLevel = outer.Width;
        // The check-time stamp: the real definition - a fresh cell or not - is written
        // once the bindings say whether the module's evaluation performs.
        var stamp = new BindingTerm.Let(ModuleStamp, MemberKind.Private, new Term.Atom(Atom.Unit.Instance));
        var inner = ExtendFromSlots(outer, stamp, [(ModuleStamp, UnitType)]) with
        {
            ScopeCaptures = outer.ScopeCaptures.Add(stampLevel),
        };

        var ((terms, entries, _), sink) = Collecting(inner, c => InferModuleBindings(c, module, stampLevel));
        Emit(ctx, sink);
        if (sink.IsEmpty) return Build(stamp, generative: false);

        var labels = terms.OfType<BindingTerm.Let>()
            .Where(l => l.Def is Term.Nominal)
            .ToDictionary(l => ((Term.Nominal)l.Def).Decl, l => l.Name);
        foreach (var decl in ctx.Metas.DeclaredNominals.Skip(firstDeclared))
            ctx.Metas.GenerativeNominals[decl] = labels.GetValueOrDefault(decl);
        return Build(stamp, generative: true);

        (Term, Value) Build(BindingTerm.Let placeholder, bool generative) =>
            (new Term.Module([placeholder with { Def = generative ? new Term.RefNew(new Term.Atom(Atom.Unit.Instance)) : new Term.Atom(Atom.Unit.Instance) }, .. terms]),
             new Value.VModule([new ModuleEntry.Field(ModuleStamp, MemberKind.Private, UnitType), .. entries], Partial: false));
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
