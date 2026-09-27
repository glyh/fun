using System.Collections.Immutable;
using Fun.Kernel;

namespace Fun.Compiler;

/// <summary>
/// Evidence that <see cref="Args"/> implement <see cref="Trait"/>: the dictionary
/// sits at <see cref="Level"/>, at dictionary type <see cref="Type"/>. <see cref="Vars"/>
/// are the impl's own type variables, the metas its head's free names bound.
/// </summary>
public sealed record TraitEvidence(TraitDecl Trait, EquatableArray<Value> Args, int Level, Value Type, EquatableArray<int> Vars = default);

/// <summary>
/// A dictionary chosen later: <see cref="Meta"/> stands for the impl of
/// <see cref="Trait"/> at <see cref="Args"/>, resolved in <see cref="Context"/> once
/// the arguments are known, at the latest when the unit's elaboration ends.
/// </summary>
public sealed record PendingEvidence(int Meta, Context Context, TraitDecl Trait, EquatableArray<Value> Args);

public sealed partial record Context
{
    /// <summary>
    /// The impls in scope, innermost first. Impls are resolved from lexical scope:
    /// a block's impl, a bound's hidden dictionary, an opened module's public impls.
    /// </summary>
    public ImmutableList<TraitEvidence> Evidence { get; init; } = ImmutableList<TraitEvidence>.Empty;

    public Context AddEvidence(TraitEvidence evidence) => this with { Evidence = Evidence.Insert(0, evidence) };
}

public static partial class Elaborator
{
    /// <summary>What an impl contributes: its trait and argument, dictionary type, and dictionary term and value.</summary>
    private sealed record ImplContribution(TraitDecl Trait, Value Arg, Value.VTraitDict DictType, Term Core, Value Value, EquatableArray<int> Vars);

    /// <summary>
    /// A trait: its one parameter is a defined entry standing for itself, and each
    /// operation type is read under it and kept as a closure over the declaring
    /// environment, applied to the trait's argument where the trait is used.
    /// </summary>
    private static TraitDecl ElaborateTrait(Context ctx, string name, string param, EquatableArray<(string Name, Syntax Type)> fields)
    {
        var seen = new HashSet<string>();
        foreach (var (field, _) in fields)
            if (!seen.Add(field)) throw new FunException($"duplicate trait field `{field}`");

        var paramCtx = ctx.Define(param, Value.VU.Instance, new Value.VVar(ctx.Width, []));
        return new TraitDecl(name, [.. fields.Select(f => (f.Name, new Closure(ctx.Environment, TypeTerm(paramCtx, f.Type))))]);
    }

    /// <summary><c>trait T(A) = sig { … }; body</c>: the trait is a definition the body sees.</summary>
    private static (Term, Value) InferTraitDef(Context ctx, Syntax.TraitDef t)
    {
        var decl = ElaborateTrait(ctx, Label(t.Name.Name), t.Param.Name, t.Fields);
        var (body, bodyType) = Infer(ctx.Define(t.Name.Name, Value.VU.Instance, new Value.VTrait(decl)), t.Body);
        return (new Term.Let(Term.U.Instance, new Term.TraitRef(decl), body), bodyType);
    }

    /// <summary>A module item <c>[pub] trait T(A) = …</c>: a member whose value is the trait.</summary>
    private static Context InferTraitBinding(Context ctx, Binding.Trait t, List<BindingTerm> terms, List<ModuleEntry> entries)
    {
        var decl = ElaborateTrait(ctx, Label(t.Name.Name), t.Param.Name, t.Fields);
        var kind = t.Public ? MemberKind.Public : MemberKind.Private;
        var term = new BindingTerm.Let(Label(t.Name.Name), kind, new Term.TraitRef(decl));
        terms.Add(term);
        entries.Add(new ModuleEntry.Field(term.Name, kind, Value.VU.Instance));
        return ExtendFromSlots(ctx, term, [(t.Name.Name, Value.VU.Instance)]);
    }

    /// <summary>The trait a path names, by the identity its value carries - never by its spelling.</summary>
    private static TraitDecl LocateTrait(Context ctx, Syntax path)
    {
        var (term, _) = Infer(ctx, path);
        return ctx.Force(ctx.Eval(term)) is Value.VTrait trait
            ? trait.Decl
            : throw new FunException("unknown trait");
    }

    /// <summary><c>Trait(Arg)</c>: the trait, its argument, and the dictionary type an impl of it provides.</summary>
    private static (TraitDecl Trait, Value Arg, Value.VTraitDict DictType) ImplDictType(Context ctx, Syntax traitPath, Syntax argSyntax)
    {
        var trait = LocateTrait(ctx, traitPath);
        var (argTerm, argType) = Infer(ctx, argSyntax);
        var arg = ctx.Eval(argTerm);
        CheckTypeLike(ctx, argType, arg);
        return (trait, arg, new Value.VTraitDict(trait, [arg], Nbe.OperationTypes(ctx.Metas, trait, arg)));
    }

    /// <summary>
    /// An impl head's free names are the impl's own type variables, bindable with
    /// no declaration: <c>impl Size(Option(A))</c> makes <c>A</c> its own. Each is
    /// pushed as a definition of a fresh meta around the head's own inference (not
    /// the body's), and the head's free occurrences are rewritten to it.
    /// </summary>
    // The cost is accepted: `impl Size(Optoin(A))` is a typo that silently becomes a
    // generic impl over two fresh variables; it never matches and surfaces at the use.
    // No scan, warning or validation for it.
    private static (Context Head, Syntax HeadSyntax, EquatableArray<int> Vars) BindHeadNames(Context ctx, Syntax head)
    {
        var free = new List<string>();
        head.Map(new SyntaxMapper
        {
            Form = form =>
            {
                switch (form)
                {
                    case Syntax.Var v when !Resolves(ctx, v.Id.Name): free.Add(v.Id.Name); break;
                    case Syntax.OpenChoice c when !Resolves(ctx, c.Name.Name, c.Opens, c.Fallback): free.Add(c.Name.Name); break;
                }
                return form;
            },
        });
        free = [.. free.Distinct()];
        if (free.Count == 0) return (ctx, head, []);

        var vars = EquatableArray<int>.Empty;
        foreach (var name in free)
        {
            var id = ctx.Metas.Fresh();
            ctx = ctx.Define(name, Value.VU.Instance, ctx.Eval(new Term.InsertedMeta(id, ctx.EntryKinds)));
            vars = vars.Add(id);
        }
        var bound = free.ToHashSet();
        var rewritten = head.Map(new SyntaxMapper
        {
            Form = form => form is Syntax.OpenChoice c && bound.Contains(c.Name.Name) ? new Syntax.Var(c.Name) : form,
        });
        return (ctx, rewritten, vars);
    }

    /// <summary>Whether a name is supplied by the context, as <see cref="Context.Locate"/> would.</summary>
    private static bool Resolves(Context ctx, string name)
    {
        try { ctx.Locate(name); return true; }
        catch (FunException) { return false; }
    }

    /// <summary>Whether an open choice is supplied, as <see cref="Context.LocateChoice"/> would.</summary>
    private static bool Resolves(Context ctx, string name, EquatableArray<string> opens, string? fallback)
    {
        try { ctx.LocateChoice(name, opens, fallback); return true; }
        catch (FunException) { return false; }
    }

    /// <summary>
    /// An impl's dictionary: every operation of the trait given exactly once, each
    /// checked at its type for this argument. The dictionary is a struct of the
    /// operations; each is elaborated in the impl's context, so the ith is shifted
    /// past the i entries the struct pushes before it.
    /// </summary>
    private static ImplContribution Contribute(Context ctx, Syntax traitPath, Syntax argSyntax, EquatableArray<(string Name, Syntax Value)> fields)
    {
        var (headCtx, head, vars) = BindHeadNames(ctx, argSyntax);
        var (trait, arg, dictType) = ImplDictType(headCtx, traitPath, head);
        var seen = new HashSet<string>();
        foreach (var (name, _) in fields)
        {
            if (!seen.Add(name)) throw new FunException($"duplicate field `{name}`");
            if (dictType.Operations.All(o => o.Name != name)) throw new FunException($"unknown trait method `{name}`");
        }
        foreach (var (name, _) in dictType.Operations)
            if (!seen.Contains(name)) throw new FunException($"missing trait field `{name}`");

        var bindings = fields.Select((f, i) => (BindingTerm)new BindingTerm.Let(f.Name, MemberKind.Public,
            Check(ctx, f.Value, dictType.Operations.Last(o => o.Name == f.Name).Type).Shift(i)));
        var core = new Term.Struct([], [.. bindings], Partial: false);
        return new ImplContribution(trait, arg, dictType, core, ctx.Eval(core), vars);
    }

    /// <summary><c>impl Trait(Arg) = module { … }; body</c>: the dictionary is an entry, and evidence, for the body.</summary>
    private static (Term, Value) InferImplDef(Context ctx, Syntax.ImplDef i)
    {
        var c = Contribute(ctx, i.TraitPath, i.Arg, i.Fields);
        var (inner, entry) = ctx.DefineAnonymous(c.DictType, c.Value);
        inner = inner.AddEvidence(new TraitEvidence(c.Trait, [c.Arg], entry.Level, c.DictType, c.Vars));
        var (body, bodyType) = Infer(inner, i.Body);
        return (new Term.Let(ctx.Quote(c.DictType), c.Core, body), bodyType);
    }

    /// <summary>
    /// A module or struct item <c>[pub] impl [name :] Trait(Arg) = …</c>: one entry,
    /// pushed through the slot list (I2), and evidence for the items after it.
    /// </summary>
    private static (Context, BindingTerm, ModuleEntry) ElaborateImplItem(Context ctx, Binding.Impl impl)
    {
        var fields = impl.Fields ?? throw new InvalidOperationException("an impl item without operations outside a signature");
        var c = Contribute(ctx, impl.TraitPath, impl.Arg, fields);
        var kind = impl.Public ? MemberKind.Public : MemberKind.Private;
        var name = impl.Name is { } written ? Label(written.Name) : null;
        var term = new BindingTerm.Impl(name, kind, c.Core, c.DictType);

        var slots = term.Slots() ?? throw new InvalidOperationException("an impl has a slot list");
        if (slots.Length != 1 || slots[0].Source is not SlotSource.Def def)
            throw new InvalidOperationException("an impl contributes one entry");
        var value = ctx.Eval(def.Term);
        var (after, entry) = ctx.DefineAnonymous(c.DictType, value);
        after = after.AddEvidence(new TraitEvidence(c.Trait, [c.Arg], entry.Level, c.DictType, c.Vars));
        return (after, term, new ModuleEntry.Impl(name, kind, c.DictType, value, c.Vars));
    }

    private static Context InferImplBinding(Context ctx, Binding.Impl impl, List<BindingTerm> terms, List<ModuleEntry> entries)
    {
        var (after, term, entry) = ElaborateImplItem(ctx, impl);
        terms.Add(term);
        entries.Add(entry);
        return after;
    }

    /// <summary>
    /// <c>name : impl Trait(Arg)</c> in a signature: the named impl the module must
    /// provide, whose member is its dictionary type.
    /// </summary>
    private static Context InferSignatureImpl(Context inner, Value self, Binding.Impl impl, List<BindingTerm> bindings)
    {
        var name = impl.Name is { } written ? Label(written.Name) : throw new InvalidOperationException("a signature's impl is named");
        var (_, _, dictType) = ImplDictType(inner, impl.TraitPath, impl.Arg);
        bindings.Add(new BindingTerm.Impl(name, MemberKind.Public, inner.Quote(dictType), Value.VU.Instance));
        return inner.DefineAnonymous(dictType, Nbe.DotValue(self, name)).Item1;
    }

    /// <summary>
    /// The traits an implicit binder's bound names - <c>[A : Eq]</c> or
    /// <c>[A : {Eq, Show}]</c> - or null when the domain is an ordinary type. A
    /// <c>{…}</c> set must list traits, each once.
    /// </summary>
    private static List<TraitDecl>? TraitBounds(Context ctx, Syntax domain)
    {
        TraitDecl? Named(Syntax form)
        {
            var (term, type) = Infer(ctx, form);
            return ctx.Force(type) is Value.VU && ctx.Force(ctx.Eval(term)) is Value.VTrait trait ? trait.Decl : null;
        }

        List<TraitDecl> traits;
        switch (domain)
        {
            case Syntax.TraitBoundSet set:
                traits = [.. set.Traits.Select(f => Named(f) ?? throw new FunException("a {…} bound lists traits"))];
                break;
            case Syntax.Var or Syntax.OpenChoice or Syntax.FieldAccess:
                if (Named(domain) is not { } single) return null;
                traits = [single];
                break;
            default:
                return null;
        }

        for (var i = 0; i < traits.Count; i++)
            if (traits.Skip(i + 1).Any(t => ReferenceEquals(t, traits[i])))
                throw new FunException($"duplicate trait bound `{traits[i].Name}`");
        return traits;
    }

    /// <summary>
    /// <c>[A : Eq + Show] -&gt; B</c>: an implicit type <c>A</c>, then one hidden
    /// dictionary per bound, each evidence for <c>A</c> in <c>B</c>.
    /// </summary>
    private static (Term, Value) InferBoundArrow(Context ctx, Syntax.Arrow arrow, List<TraitDecl> traits)
    {
        var arg = new Value.VVar(ctx.Width, []);
        var inner = ctx.Bind(arrow.Name!.Name, Value.VU.Instance);
        var dicts = new List<Term>();
        foreach (var trait in traits)
        {
            var dictType = new Value.VTraitDict(trait, [arg], Nbe.OperationTypes(ctx.Metas, trait, arg));
            dicts.Add(inner.Quote(dictType));
            (inner, var entry) = inner.BindAnonymous(dictType);
            inner = inner.AddEvidence(new TraitEvidence(trait, [arg], entry.Level, dictType));
        }
        var body = TypeTerm(inner, arrow.Codomain);
        for (var i = dicts.Count - 1; i >= 0; i--) body = new Term.Pi(Explicitness.Implicit, dicts[i], body);
        return (new Term.Pi(Explicitness.Implicit, Term.U.Instance, body), Value.VU.Instance);
    }

    /// <summary>
    /// Checking a lambda's body against a type that next takes hidden dictionaries:
    /// each is bound, as evidence for the body. The count is how many lambdas the
    /// body's term is wrapped in.
    /// </summary>
    private static (Context, Value, int) InsertHiddenDicts(Context ctx, Value type)
    {
        var count = 0;
        while (ctx.Force(type) is Value.VPi { Explicitness: Explicitness.Implicit } pi && ctx.Force(pi.Domain) is Value.VTraitDict dict)
        {
            (ctx, var entry) = ctx.BindAnonymous(dict);
            ctx = ctx.AddEvidence(new TraitEvidence(dict.Decl, dict.Args, entry.Level, dict));
            type = Nbe.ApplyClosure(ctx.Metas, pi.Codomain, new Value.VVar(entry.Level, []));
            count++;
        }
        return (ctx, type, count);
    }

    /// <summary>A candidate impl for a use: its head arguments, its own variables, and the dictionary it offers.</summary>
    private sealed record EvidenceCandidate(EquatableArray<Value> Args, EquatableArray<int> Vars, Term Term, Value Type);

    /// <summary>
    /// The impl in scope for <paramref name="trait"/> at <paramref name="args"/>
    /// (traits.md, "Resolution"): evidence whose arguments match, and a struct
    /// argument's own public impls, of which the most precise is chosen. Null when
    /// there is none; no unique most precise one is an ambiguity. An argument whose
    /// type is not yet known (a meta) makes the choice wait.
    /// </summary>
    private static (Term Term, Value Type)? ResolveEvidence(Context ctx, TraitDecl trait, EquatableArray<Value> args)
    {
        // An unknown argument type waits (rule 4): a candidate matching it could still
        // be beaten by a later, more precise one, so resolving now would be a guess.
        if (args.Any(a => ctx.Force(a) is Value.VMeta or Value.VNeutral)) return null;

        // P is more precise than Q when P's arguments are an instance of Q's: Q's own
        // variables are filled in to give P's (rule 2).
        bool Instance(EvidenceCandidate p, EvidenceCandidate q) => Matches(ctx, q.Args, p.Args, q.Vars);

        var candidates = ctx.Evidence
            .Where(e => ReferenceEquals(e.Trait, trait) && Matches(ctx, e.Args, args, e.Vars))
            .Select(e => new EvidenceCandidate(e.Args, e.Vars,
                new Term.Var(Nbe.LevelToIndex(ctx.Width, e.Level)), e.Type))
            .ToList();

        if (args.Length == 1 && ctx.Force(args[0]) is Value.VStruct st)
            candidates.AddRange(st.Entries.OfType<ModuleEntry.Impl>()
                .Where(i => i.Kind == MemberKind.Public && ctx.Force(i.DictType) is Value.VTraitDict d
                            && ReferenceEquals(d.Decl, trait) && Matches(ctx, d.Args, args, i.Vars))
                .Select(i => new EvidenceCandidate(
                    ((Value.VTraitDict)ctx.Force(i.DictType)).Args, i.Vars, ctx.Quote(i.Value), i.DictType)));

        if (candidates.Count == 0) return null;
        var best = candidates.Where(p => candidates.All(q => ReferenceEquals(p, q) || Instance(p, q))).ToList();
        return best.Count == 1
            ? (best[0].Term, best[0].Type)
            : throw new FunException($"ambiguous implementation of `{trait.Name}`");
    }

    /// <summary>
    /// The dictionary for <paramref name="trait"/> at <paramref name="args"/>: the impl
    /// chosen now, or - while an argument is not yet known - a meta the choice fills
    /// once it is (<see cref="ResolvePendingEvidence"/>). A known argument with no impl
    /// is an error.
    /// </summary>
    private static Term Evidence(Context ctx, TraitDecl trait, EquatableArray<Value> args)
    {
        if (ResolveEvidence(ctx, trait, args) is { } found) return found.Term;
        if (!args.Any(a => Unresolved(ctx, a))) throw new FunException($"missing implementation of `{trait.Name}`");
        var meta = ctx.Metas.Fresh();
        ctx.Metas.PendingEvidence.Add(new PendingEvidence(meta, ctx, trait, args));
        return new Term.InsertedMeta(meta, ctx.EntryKinds);
    }

    /// <summary>
    /// The choices that waited on argument types, made now that elaboration of the
    /// unit has ended: each argument is known and one impl matches, or it is an error.
    /// Only the choices this unit made (metas from <paramref name="since"/>) must be
    /// settled; an importer's pending choices wait for the importer.
    /// </summary>
    public static void ResolvePendingEvidence(MetaContext metas, int since)
    {
        foreach (var pending in metas.PendingEvidence.Where(p => p.Meta >= since).ToList())
        {
            metas.PendingEvidence.Remove(pending);
            var ctx = pending.Context;
            var found = ResolveEvidence(ctx, pending.Trait, pending.Args)
                ?? throw new FunException(pending.Args.Any(a => ctx.Force(a) is Value.VMeta)
                    ? $"cannot choose an implementation of `{pending.Trait.Name}`: its argument type is never known"
                    : $"missing implementation of `{pending.Trait.Name}`");
            // The chosen dictionary is one value whatever the meta is applied to (levels
            // are stable), so its solution ignores the spine: a lambda per bound entry.
            Term solution = new Term.Imported(ctx.Eval(found.Term));
            foreach (var kind in ctx.EntryKinds)
                if (kind == EntryKind.Bound) solution = new Term.Lam(solution);
            metas.Solve(pending.Meta, Nbe.Eval(metas, Environment.Empty, solution));
        }
    }

    /// <summary>Whether an argument is not yet known well enough to say no impl exists for it.</summary>
    private static bool Unresolved(Context ctx, Value arg) => ctx.Force(arg) is Value.VMeta or Value.VVar or Value.VNeutral;

    /// <summary>
    /// Whether an impl head's arguments match a use's: read-back equality, or a
    /// structural unification that solves only the impl's own variables
    /// (<paramref name="vars"/>) - the use's metas and variables stay rigid - and is
    /// undone, so the impl stays generic. The no-solve guard beyond those variables
    /// is what makes it a match rather than a guess: it admits width subtyping and
    /// eta, but never commits to the use's unknowns, so a choice still waits on an
    /// unknown argument type (traits.md, "Resolution", rule 4).
    /// </summary>
    private static bool Matches(Context ctx, EquatableArray<Value> pattern, EquatableArray<Value> target, EquatableArray<int> vars)
    {
        if (pattern.Length != target.Length) return false;
        var before = ctx.Metas.Snapshot();
        var ok = true;
        try
        {
            foreach (var (p, t) in pattern.Zip(target))
                if (!Nbe.Convertible(ctx.Metas, ctx.Width, p, t) && !Unify.TryValues(ctx.Metas, ctx.Width, p, t))
                { ok = false; break; }
            if (ok)
            {
                var after = ctx.Metas.Snapshot();
                for (var i = 0; i < before.Length && ok; i++)
                    if (before[i] is null && after[i] is not null && !vars.Contains(i)) ok = false;
            }
        }
        catch (FunException) { ok = false; }
        finally { ctx.Metas.Restore(before); }
        return ok;
    }

    /// <summary>The trait a checked form's value is, when it is one.</summary>
    private static TraitDecl? TraitOf(Context ctx, Term term, Value type) =>
        ctx.Force(type) is Value.VU && ctx.Force(ctx.Eval(term)) is Value.VTrait trait ? trait.Decl : null;

    /// <summary>
    /// <c>Trait.op</c>: generic over the trait's argument, taking the dictionary for it
    /// as a hidden argument - <c>[A : Type] -&gt; [Trait(A)] -&gt; op's type at A</c> - so
    /// the impl is chosen at the use's argument type exactly as a bound's is.
    /// </summary>
    private static (Term, Value) TraitMethod(Context ctx, TraitDecl trait, string name)
    {
        var arg = new Value.VVar(ctx.Width, []);
        var dictType = new Value.VTraitDict(trait, [arg], Nbe.OperationTypes(ctx.Metas, trait, arg));
        if (dictType.Operations.All(o => o.Name != name)) throw new FunException($"unknown trait method `{name}`");
        var opType = dictType.Operations.Last(o => o.Name == name).Type;
        var type = new Term.Pi(Explicitness.Implicit, Term.U.Instance,
            new Term.Pi(Explicitness.Implicit, Nbe.Quote(ctx.Metas, ctx.Width + 1, dictType),
                Nbe.Quote(ctx.Metas, ctx.Width + 2, opType)));
        return (new Term.Lam(new Term.Lam(new Term.Dot(new Term.Var(0), name))), ctx.Eval(type));
    }

    /// <summary>
    /// An application whose function next takes hidden dictionaries: each waits on
    /// a placeholder until the explicit argument is checked (solving the type
    /// arguments), then is resolved. An argument still unknown takes a meta; a known
    /// one with no impl is an error.
    /// </summary>
    private static (Term, Value)? InferApWithPendingDicts(Context ctx, Term fn, Value fnType, Syntax argSyntax)
    {
        var pending = new List<Value.VTraitDict>();
        var type = fnType;
        while (ctx.Force(type) is Value.VPi { Explicitness: Explicitness.Implicit } pi && ctx.Force(pi.Domain) is Value.VTraitDict dict)
        {
            pending.Add(dict);
            type = Nbe.ApplyClosure(ctx.Metas, pi.Codomain, ctx.RawMeta());
        }
        if (pending.Count == 0 || ctx.Force(type) is not Value.VPi { Explicitness: Explicitness.Explicit } explicitPi) return null;

        var (arg, argEffects) = Collecting(ctx, c => Check(c, argSyntax, explicitPi.Domain));
        Emit(ctx, argEffects);
        foreach (var dict in pending)
            fn = new Term.Ap(fn, Explicitness.Implicit, Evidence(ctx, dict.Decl, dict.Args));
        // The argument's value is read only when evaluating it is safe: one that
        // performs takes a rigid stand-in as the codomain's argument.
        var result = Nbe.ApplyClosure(ctx.Metas, explicitPi.Codomain, ArgumentValue(ctx, arg, argEffects));
        return (EmitLatent(ctx, explicitPi, new Term.Ap(fn, Explicitness.Explicit, arg)), ctx.Force(result));
    }

    /// <summary>
    /// What <c>open</c> pushes for a public impl of the module: an entry holding the
    /// dictionary, and evidence - unless the same impl is already evidence, since
    /// opening a module twice brings one impl, not two.
    /// </summary>
    private static Context OpenImpl(Context ctx, ModuleEntry.Impl impl, Value module, int index, List<OpenMember> opened)
    {
        var member = new OpenMember.Impl(index, impl.Name);
        var value = Nbe.OpenedImpl(module, member);
        var duplicate = ctx.Force(impl.DictType) is Value.VTraitDict dict && ctx.Evidence.Any(e =>
            ReferenceEquals(e.Trait, dict.Decl)
            && e.Args.Length == dict.Args.Length
            && e.Args.Zip(dict.Args).All(p => Nbe.Convertible(ctx.Metas, ctx.Width, p.First, p.Second))
            && Nbe.Convertible(ctx.Metas, ctx.Width, ctx.Environment[Nbe.LevelToIndex(ctx.Width, e.Level)], value));
        var (after, entry) = ctx.DefineAnonymous(impl.DictType, value);
        opened.Add(member);
        return !duplicate && after.Force(impl.DictType) is Value.VTraitDict d
            ? after.AddEvidence(new TraitEvidence(d.Decl, d.Args, entry.Level, impl.DictType, impl.Vars))
            : after;
    }
}
