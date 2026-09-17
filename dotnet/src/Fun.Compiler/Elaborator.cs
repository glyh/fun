using System.Collections.Immutable;
using Fun.Kernel;

namespace Fun.Compiler;

/// <summary>A name's entry in a scope: the level it was pushed at, and its type.</summary>
public sealed record Entry(int Level, Value Type);

/// <summary>
/// A context: one ordered sequence of entries viewed through columns that must
/// stay the same length, its <see cref="Width"/>. <see cref="Environment"/> is the column
/// the evaluator receives; <see cref="EntryKinds"/> is the mask a meta is
/// abstracted over. An entry's type lives in <see cref="Names"/>, keyed by the
/// resolved name expansion gave its binder.
/// </summary>
public sealed partial record Context(
    Environment Environment,
    int Width,
    EquatableArray<EntryKind> EntryKinds,
    ImmutableDictionary<string, Entry> Names,
    MetaContext Metas,
    bool PreludeOpen)
{
    /// <summary>Each open entered, by label, with the members it brought in. An open choice locates a name here.</summary>
    public ImmutableDictionary<string, ImmutableDictionary<string, Entry>> Opened { get; init; } =
        ImmutableDictionary<string, ImmutableDictionary<string, Entry>>.Empty;

    /// <summary>The base context's names: where an open choice with no candidate and no binder is located.</summary>
    public ImmutableDictionary<string, Entry> BaseNames { get; init; } = ImmutableDictionary<string, Entry>.Empty;

    public static Context Empty(MetaContext metas, bool preludeOpen) =>
        new(Environment.Empty, 0, [], ImmutableDictionary<string, Entry>.Empty, metas, preludeOpen);

    /// <summary>Pushes a defined entry no name reaches directly: an opened member, located through its open.</summary>
    public (Context, Entry) DefineAnonymous(Value type, Value value) =>
        (this with
        {
            Environment = Environment.Push(value),
            Width = Width + 1,
            EntryKinds = EntryKinds.Insert(0, EntryKind.Defined),
        }, new Entry(Width, type));

    /// <summary>Pushes a bound variable: its value is unknown, so it stands as itself.</summary>
    public Context Bind(string name, Value type) => this with
    {
        Environment = Environment.Push(new Value.VVar(Width, [])),
        Width = Width + 1,
        EntryKinds = EntryKinds.Insert(0, EntryKind.Bound),
        Names = Names.SetItem(name, new Entry(Width, type)),
    };

    /// <summary>Pushes a definition: its value is known.</summary>
    public Context Define(string name, Value type, Value value) => this with
    {
        Environment = Environment.Push(value),
        Width = Width + 1,
        EntryKinds = EntryKinds.Insert(0, EntryKind.Defined),
        Names = Names.SetItem(name, new Entry(Width, type)),
    };

    /// <summary>
    /// The entry a name denotes: a binder by the resolved name expansion gave
    /// it, else the base context. Inside the prelude's open a name the base
    /// lacks may be one of `std`'s members, which are not ported, so it is not
    /// yet known to be unbound.
    /// </summary>
    // Checking the base before `std` gives the prototype's answer (opens first,
    // then the base) only while `std` rebinds no base-context name, which holds
    // today: the base names it spells are members of its `Syntax` module.
    public (int Index, Value Type) Locate(string name)
    {
        if (Names.TryGetValue(name, out var entry)) return At(entry);
        throw Unbound(name, PreludeOpen);
    }

    /// <summary>
    /// A name nothing supplies. Where the prelude is open, it may be one stage 2
    /// publishes, which is not ported: then it is not yet known to be unbound.
    /// </summary>
    private static Exception Unbound(string name, bool preludeOpen)
    {
        var written = name.IndexOf('#') is var i and >= 0 ? name[..i] : name;
        return preludeOpen && Prelude.Stage2Names.Contains(written)
            ? new NotImplementedException($"not ported yet: `{written}` from the prelude")
            : new FunException($"unbound variable: {written}");
    }

    /// <summary>
    /// An open choice: the first candidate open that has the member, else the
    /// binder it shadows, else the base context.
    /// </summary>
    public (int Index, Value Type) LocateChoice(string name, EquatableArray<string> opens, string? fallback)
    {
        foreach (var label in opens)
            if (Opened.TryGetValue(label, out var members) && members.TryGetValue(name, out var member))
                return At(member);
        if (fallback is not null) return Locate(fallback);
        if (BaseNames.TryGetValue(name, out var based)) return At(based);
        throw Unbound(name, PreludeOpen || opens.Contains(Fun.Expand.Expander.UnitOpenLabel(Prelude.Path)));
    }

    private (int Index, Value Type) At(Entry entry) => (Nbe.LevelToIndex(Width, entry.Level), entry.Type);

    public Value Eval(Term term) => Nbe.Eval(Metas, Environment, term);
    public Term Quote(Value value) => Nbe.Quote(Metas, Width, value);
    public Value Force(Value value) => Nbe.Force(Metas, value);

    /// <summary>A meta with no spine, standing for a type not written.</summary>
    public Value RawMeta() => new Value.VMeta(Metas.Fresh(), []);

    public void Unify(Value expected, Value inferred)
    {
        try
        {
            // One request: every step of the unification spends from the same budget.
            Metas.Budget.Request("a unification", () =>
            {
                Fun.Compiler.Unify.Values(Metas, Width, expected, inferred);
                return true;
            });
        }
        catch (UnifyException e)
        {
            throw new FunException($"type mismatch: {e.Message}");
        }
    }
}

/// <summary>
/// Bidirectional elaboration of expanded syntax into core terms: <see cref="Infer"/>
/// synthesises a type, <see cref="Check"/> verifies against one.
/// </summary>
public static partial class Elaborator
{
    /// <summary>
    /// The base context every compilation unit elaborates against: the builtins and
    /// the prelude bound as <c>stdlib</c> - bound, not opened. A program's indices
    /// count these entries, so it runs in this context's environment.
    /// </summary>
    public static Context BaseContext(MetaContext metas, bool preludeOpen)
    {
        metas.SeedFrom(Prelude.Metas);
        var ctx = BuiltinContext(metas, preludeOpen);
        var (value, type) = Prelude.Unit;
        ctx = ctx.Define(Prelude.Binding, type, value);
        return ctx with { BaseNames = ctx.Names };
    }

    /// <summary>The atom types, the primitives and the reference entries: what the prelude itself elaborates against.</summary>
    public static Context BuiltinContext(MetaContext metas, bool preludeOpen)
    {
        var ctx = Context.Empty(metas, preludeOpen);
        foreach (var (name, ty) in new (string, AtomTy)[]
                 {
                     ("I64", AtomTy.I64), ("Unit", AtomTy.Unit), ("Char", AtomTy.Char),
                     ("String", AtomTy.String), ("Scopes", AtomTy.Scopes), ("Absurd", AtomTy.Absurd),
                 })
            ctx = ctx.Define(name, Value.VU.Instance, new Value.VAtomTy(ty));
        ctx = ctx.Define("Type", Value.VU.Instance, Value.VU.Instance);
        ctx = ctx.Define("EffectRow", Value.VU.Instance, Value.VEffectRowTy.Instance);
        ctx = DefineReferenceEntries(ctx);
        // Each primitive a program names: a defined entry whose value is the primitive itself.
        foreach (var p in Primitives.Declarations)
            if (p.Type is { } type) ctx = ctx.Define(p.Name, type, new Value.VNeutral(type, new Head.HPrim(p.Name), []));
        return ctx with { BaseNames = ctx.Names };
    }

    /// <summary>
    /// Elaborates a program read as an expression. Such a program has nowhere to
    /// write <c>open (import "std")</c>, so it is elaborated inside that open.
    /// </summary>
    public static Elaborated ElaborateProgram(Syntax program, Loader? loader = null, Fun.Expand.Expander? expander = null)
    {
        var metas = new MetaContext();
        var ctx = BaseContext(metas, preludeOpen: true) with { Loader = loader, Expander = expander };
        // A program's entry leaves nothing unhandled: that is an error, not a run-time crash.
        var ((term, type), performed) = Collecting(ctx, c => Infer(c, program));
        RequireHandledAtEntry(ctx, performed, since: 0);
        return new Elaborated(term, type, ctx);
    }

    public static (Term, Value) Infer(Context ctx, Syntax stx)
    {
        switch (stx)
        {
            case Syntax.Atom a:
                return (new Term.Atom(a.Value), new Value.VAtomTy(a.Value switch
                {
                    Atom.I64 => AtomTy.I64,
                    Atom.Unit => AtomTy.Unit,
                    Atom.Char => AtomTy.Char,
                    Atom.Str => AtomTy.String,
                    Atom.Scopes => AtomTy.Scopes,
                    _ => throw new InvalidOperationException($"unhandled atom {a.Value.GetType().Name}"),
                }));

            case Syntax.Var v:
            {
                var (index, type) = ctx.Locate(v.Id.Name);
                return (new Term.Var(index), type);
            }

            case Syntax.OpenChoice choice:
            {
                var (index, type) = ctx.LocateChoice(choice.Name.Name, choice.Opens, choice.Fallback);
                return (new Term.Var(index), type);
            }

            case Syntax.Import import:
                return InferImport(ctx, import);

            case Syntax.Module module:
                return InferModule(ctx, module);

            case Syntax.Match match: return InferMatch(ctx, match);
            case Syntax.EffectDef def: return InferEffectDef(ctx, def);
            case Syntax.Perform perform: return InferPerform(ctx, perform);
            case Syntax.Resume resume: return InferResume(ctx, resume);
            case Syntax.RefNew or Syntax.RefGet or Syntax.RefSet: return InferRefs(ctx, stx);
            case Syntax.TraitDef trait: return InferTraitDef(ctx, trait);
            case Syntax.ImplDef impl: return InferImplDef(ctx, impl);
            case Syntax.TraitBoundSet: throw new FunException("a {…} bound lists traits");
            case Syntax.Enum e: return InferEnum(ctx, e);
            case Syntax.PatternSynonym s: return InferPatternSynonym(ctx, s);
            case Syntax.Quote or Syntax.QuoteDecls: return InferQuote(ctx, stx);
            case Syntax.MacroCall call: return ApplyTypedMacro(ctx, call, null);

            case Syntax.Open open:
            {
                var (body, of, members) = OpenModule(ctx, open.Of, open.Label, open.RolesInRegion);
                var (bodyTerm, bodyType) = Infer(body, open.Body);
                return (new Term.Open(of, members, bodyTerm), bodyType);
            }

            case Syntax.FieldAccess access:
            {
                var ((of, ofType), headPerformed) = Collecting(ctx, c => Infer(c, access.Of));
                Emit(ctx, headPerformed);
                if (TraitOf(ctx, of, ofType) is { } trait) return TraitMethod(ctx, trait, access.Field);
                // A nominal type or type former's members are its constructors.
                if (ctx.Force(ofType) is Value.VU or Value.VPi && ConstructorMember(ctx, of, ofType, access.Field) is { } constructor)
                    return constructor;
                var member = InferMember(ctx, of, ofType, access.Field);
                CheckGenerativeEscape(ctx, headPerformed, access.Field, member.Item2);
                return member;
            }

            case Syntax.Struct st: return InferStruct(ctx, st);
            case Syntax.RecordConstruct record: return InferRecordConstruct(ctx, record);
            case Syntax.Sig sig: return InferSig(ctx, sig);
            case Syntax.Self: return ctx.LocateSelf();
            case Syntax.SelfType: return (ctx.Quote(ctx.SelfType ?? throw new FunException("unbound variable: Self")), Value.VU.Instance);

            case Syntax.Annotated a:
            {
                var type = TypeValue(ctx, a.Type);
                return (Check(ctx, a.Inner, type), type);
            }

            case Syntax.Lam lam when LambdaHasPoly(ctx, lam): return Infer(ctx, PolyLambda(ctx, lam));
            case Syntax.Lam lam:
                return InferLam(ctx, lam);

            case Syntax.Ap { Explicitness: Explicitness.Explicit } ap:
                return InferAp(ctx, ap);

            case Syntax.Ap { Explicitness: Explicitness.Implicit } ap:
                return InferApImplicit(ctx, ap);

            case Syntax.Let { Recursive: true } let:
                return Discharging(ctx, c => InferRecLet(c, let));

            case Syntax.LetRecGroup group:
                return InferLetRecGroup(ctx, group);

            case Syntax.Let { Recursive: false } let:
                return Discharging(ctx, c => InferLet(c, let));

            case Syntax.Arrow poly when HasPoly(ctx, poly): return Infer(ctx, PolySignature(ctx, poly));

            case Syntax.Arrow { Explicitness: Explicitness.Implicit, Name: not null, Row: null } bounded
                when TraitBounds(ctx, bounded.Domain) is { } traits:
                return InferBoundArrow(ctx, bounded, traits);

            case Syntax.Arrow arrow:
            {
                var domain = TypeTerm(ctx, arrow.Domain);
                var inner = ctx.Bind(arrow.Name?.Name ?? "_", ctx.Eval(domain));
                var row = ElaborateRow(inner, arrow.Row);
                var codomain = TypeTerm(inner, arrow.Codomain);
                return (new Term.Pi(arrow.Explicitness, domain, codomain) { Row = row }, Value.VU.Instance);
            }

            case Syntax.Prod prod:
            {
                var items = prod.Items.Select(i => Infer(ctx, i)).ToEquatableArray();
                return (new Term.Prod([.. items.Select(i => i.Item1)]),
                        new Value.VProdTy([.. items.Select(i => i.Item2)]));
            }

            case Syntax.Proj proj:
            {
                var (of, ofType) = Infer(ctx, proj.Of);
                if (ctx.Force(ofType) is not Value.VProdTy tuple)
                    throw new FunException("projection of a non-tuple");
                if (proj.Index < 0 || proj.Index >= tuple.Items.Length)
                    throw new FunException("tuple length mismatch");
                return (new Term.Proj(of, proj.Index), ctx.Force(tuple.Items[proj.Index]));
            }

            default:
                throw new NotImplementedException($"not ported yet: elaborating {stx.GetType().Name}");
        }
    }

    public static Term Check(Context ctx, Syntax stx, Value expected)
    {
        expected = ctx.Force(expected);
        switch (stx, expected)
        {
            case (Syntax.Lam lam, _) when LambdaHasPoly(ctx, lam): return Check(ctx, PolyLambda(ctx, lam), expected);
            case (_, Value.VPi { Explicitness: Explicitness.Implicit } rowPi)
                when ctx.Force(rowPi.Domain) is Value.VEffectRowTy && stx is not Syntax.Lam { Param.Explicitness: Explicitness.Implicit }:
                return CheckUnderImplicitRow(ctx, stx, rowPi);

            case (_, Value.VPi { Explicitness: Explicitness.Implicit } implicitPi)
                when stx is not Syntax.Lam { Param.Explicitness: Explicitness.Implicit }:
                return CheckUnderImplicit(ctx, stx, implicitPi);

            case (Syntax.Match match, _): return CheckMatch(ctx, match, expected);
            case (Syntax.MacroCall call, _): return ApplyTypedMacro(ctx, call, expected).Item1;
            case (Syntax.QuoteDecls quote, _) when CheckQuoteDecl(ctx, quote, expected) is { } decl: return decl;

            case (Syntax.Lam lam, Value.VPi pi):
            {
                if (lam.Param.Explicitness != pi.Explicitness)
                    throw new FunException("applying non-function");
                // A written parameter type is an annotation like any other: it
                // must agree with the domain expected. The prototype ignores it
                // here (lambda-check-ignores-written-parameter-type).
                if (lam.Param.Type is { } written)
                    ctx.Unify(pi.Domain, TypeValue(ctx, written));
                var binder = new Value.VVar(ctx.Width, []);
                var inner = ctx.Bind(lam.Param.Name.Name, pi.Domain) with { Enclosing = lam.Body, HandlerScopes = [] };
                var bodyType = Nbe.ApplyClosure(ctx.Metas, pi.Codomain, binder);
                var (dictCtx, dictBodyType, hidden) = InsertHiddenDicts(inner, bodyType);
                // The body performs within the row the function type declares; a bare arrow's is empty.
                var since = ctx.Metas.Count;
                var (body, performed) = Collecting(dictCtx, c => Check(c, lam.Body, dictBodyType));
                performed = DischargeLocalHeaps(dictCtx, since, [pi.Domain, dictBodyType], performed);
                CheckEffectSubset(dictCtx, performed, Nbe.EvalRowClosure(ctx.Metas, pi.Row, binder), inFunction: true);
                for (var i = 0; i < hidden; i++) body = new Term.Lam(body);
                return new Term.Lam(body);
            }

            case (Syntax.Prod prod, Value.VProdTy tuple):
                if (prod.Items.Length != tuple.Items.Length) throw new FunException("tuple length mismatch");
                return new Term.Prod([.. prod.Items.Zip(tuple.Items, (i, t) => Check(ctx, i, t))]);

            default:
            {
                var (term, inferred) = Infer(ctx, stx);
                (term, inferred) = InsertImplicitArgs(ctx, term, inferred);
                AgreeWithExpected(ctx, expected, inferred, term);
                return term;
            }
        }
    }

    /// <summary>
    /// A module: each binding elaborates in the context the ones before it
    /// built, pushing exactly its slots (I2). Its type lists every binding's
    /// member, private ones included, so widths stay aligned; only public ones
    /// are reachable from outside.
    /// </summary>
    // ponytail: no module stamp slot yet; it arrives with nominals (E11), and
    // both sides read the slot list, so adding it moves no index by hand.
    private static (Term, Value) InferModule(Context ctx, Syntax.Module module) =>
        Generative(ctx, c => InferModuleBindings(c, module));

    private static (Term, Value, IReadOnlyList<BindingTerm>) InferModuleBindings(Context ctx, Syntax.Module module)
    {
        var inner = ctx.WithoutSelf() with { Enclosing = module };
        var performingMember = false;
        var terms = new List<BindingTerm>();
        var entries = new List<ModuleEntry>();
        var clashes = new ExportClashes();

        foreach (var binding in module.Bindings)
        {
            var before = entries.Count;
            inner = ElaborateBinding(inner, binding, terms, entries, ref performingMember);
            clashes.Check(binding, entries.Skip(before));
        }

        // A member whose value performed is a rigid entry of the module: no member's type may name it.
        if (performingMember)
            foreach (var field in entries.OfType<ModuleEntry.Field>())
                CheckSealedStays(inner, ctx.Width, inner.Width, field.Name, field.Value);
        return (new Term.Module([.. terms]), new Value.VModule([.. entries], Partial: false), terms);
    }

    /// <summary>
    /// One binding of a module, elaborated in the context the bindings before it built:
    /// its terms and entries are appended, and the context it pushed is returned.
    /// </summary>
    private static Context ElaborateBinding(Context inner, Binding binding, List<BindingTerm> terms, List<ModuleEntry> entries, ref bool performingMember)
    {
        switch (binding)
        {
            case Binding.Export export:
                return InferExport(inner, export, terms, entries);

            case Binding.RecGroup group:
                return InferRecGroupBinding(inner, group, terms, entries);

            case Binding.Effect effect:
                return InferEffectBinding(inner, effect, terms, entries);

            case Binding.Trait trait:
                return InferTraitBinding(inner, trait, terms, entries);

            case Binding.Impl impl:
                return InferImplBinding(inner, impl, terms, entries);

            case Binding.Let let:
            {
                var ((def, type), performed) = Collecting(inner, c => let.Recursive ? InferRecMember(c, let) : Infer(c, let.Value));
                Emit(inner, performed);
                var kind = let.Public ? MemberKind.Public : MemberKind.Private;
                var term = new BindingTerm.Let(Label(let.Name.Name), kind, def);
                // A value that performs is not known at check time (E4): its binder is a rigid
                // entry, whose type seals what a generative module declared (E11).
                if (!performed.IsEmpty) (type, performingMember) = (Seal(inner, type), true);
                terms.Add(term);
                entries.Add(new ModuleEntry.Field(term.Name, kind, type));
                return performed.IsEmpty
                    ? ExtendFromSlots(inner, term, [(let.Name.Name, type)])
                    : BindFromSlots(inner, term, [(let.Name.Name, type)]);
            }

            case Binding.Open open:
            {
                var (after, of, members) = OpenModule(inner, open.Of, open.Label, open.RolesInRegion);
                terms.Add(new BindingTerm.Open(of, members));
                return after;
            }

            default:
                throw new NotImplementedException($"not ported yet: elaborating the binding {binding.GetType().Name}");
        }
    }

    /// <summary>
    /// A unit's top-level binding, just expanded, elaborated into the unit's context as
    /// of here: what a macro defined after it compiles against (M3). The whole unit is
    /// elaborated again once it is expanded; this context only serves macro definitions.
    /// </summary>
    internal static Context AdvanceUnit(Context unit, Binding binding)
    {
        var performing = false;
        return ElaborateBinding(unit, binding, [], [], ref performing);
    }

    /// <summary>
    /// Pushes a binding's slots, each with the key it is located by and its type;
    /// its value is the slot's term evaluated in the context so far. The payloads
    /// must match the slot list one for one.
    /// </summary>
    private static Context ExtendFromSlots(Context ctx, BindingTerm binding, EquatableArray<(string Key, Value Type)> payloads)
    {
        var slots = binding.Slots() ?? throw new InvalidOperationException("an open has no slot list");
        if (slots.Length != payloads.Length)
            throw new InvalidOperationException($"{slots.Length} slots against {payloads.Length} payloads");
        for (var i = 0; i < slots.Length; i++)
        {
            var value = slots[i].Source switch
            {
                SlotSource.Def d => ctx.Eval(d.Term),
                var other => throw new InvalidOperationException($"unhandled slot source {other.GetType().Name}"),
            };
            ctx = ctx.Define(payloads[i].Key, payloads[i].Type, value);
        }
        return ctx;
    }

    /// <summary>
    /// Pushes a binding's slots as bound entries: their values are not known at check
    /// time. Same slot list, so the same width, as <see cref="ExtendFromSlots"/>.
    /// </summary>
    private static Context BindFromSlots(Context ctx, BindingTerm binding, EquatableArray<(string Key, Value Type)> payloads)
    {
        var slots = binding.Slots() ?? throw new InvalidOperationException("an open has no slot list");
        if (slots.Length != payloads.Length)
            throw new InvalidOperationException($"{slots.Length} slots against {payloads.Length} payloads");
        return payloads.Aggregate(ctx, (c, p) => c.Bind(p.Key, p.Type));
    }

    /// <summary>
    /// Opens a module: one entry per public member of its *type*, in order, each
    /// valued by projecting the module. Recorded under the open's label for the
    /// open choices in its region. A member named like a syntactic role visible in
    /// the region is an error: the role would read that name, never the member (M7).
    /// </summary>
    private static (Context, Term, EquatableArray<OpenMember>) OpenModule(
        Context ctx, Syntax of, string label, EquatableArray<string> rolesInRegion)
    {
        var (term, inferred) = Infer(ctx, of);
        if (OpenNominal(ctx, term, inferred, label) is { } nominal) return nominal;
        // A signature-typed module (a parameter) opens as the signature gives it.
        var type = ModuleTypeOf(ctx, inferred, term);
        if (ctx.Force(type) is not Value.VModule moduleType)
            throw ctx.Force(type) is Value.VMeta or Value.VVar or Value.VNeutral
                ? new NotImplementedException("not ported yet: opening a value of unknown type")
                : new FunException("open of a non-module");

        var value = ctx.Eval(term);
        var members = ImmutableDictionary<string, Entry>.Empty;
        var opened = new List<OpenMember>();
        var impls = 0;
        foreach (var member in moduleType.Entries)
        {
            if (member is ModuleEntry.Impl { Kind: MemberKind.Public } impl)
            {
                ctx = OpenImpl(ctx, impl, value, impls++, opened);
                continue;
            }
            if (member is not ModuleEntry.Field { Kind: MemberKind.Public } field) continue;
            (ctx, var entry) = ctx.DefineAnonymous(field.Value, Nbe.DotValue(value, field.Name));
            if (field.Constructor is { } mark)
                ctx = ctx with { ConstructorEntries = ctx.ConstructorEntries.SetItem(entry.Level, (mark.Type, mark.TypeType, mark.Constructor)) };
            members = members.SetItem(field.Name, entry);
            opened.Add(new OpenMember.Field(field.Name));
        }
        if (rolesInRegion.FirstOrDefault(members.ContainsKey) is { } supplied)
            throw new FunException($"the open supplies `{supplied}`, which a syntactic role names in its region");
        return (ctx with { Opened = ctx.Opened.SetItem(label, members) }, term, [.. opened]);
    }

    /// <summary>The written name a resolved name was minted from: a member's label.</summary>
    private static string Label(string resolved) =>
        resolved.IndexOf('#') is var i and >= 0 ? resolved[..i] : resolved;

    /// <summary>
    /// <c>fn(x) { body }</c> with no expected type. An unwritten parameter type
    /// is a fresh meta; the codomain is the body's type read back under
    /// the parameter.
    /// </summary>
    private static (Term, Value) InferLet(Context ctx, Syntax.Let let)
    {
        var writtenType = let.Type is { } written ? TypeValue(ctx, written) : null;
        var ((valueTerm, valueType), performed) = Collecting(ctx, c =>
            writtenType is null ? Infer(c, let.Value) : (Check(c, let.Value, writtenType), writtenType));
        Emit(ctx, performed);
        (valueTerm, valueType) = Generalise(ctx, valueTerm, valueType);
        // A value is known in the body only when evaluating it performs nothing (E4).
        var body = performed.IsEmpty
            ? ctx.Define(let.Name.Name, valueType, ctx.Eval(valueTerm))
            : ctx.Bind(let.Name.Name, Seal(ctx, valueType));
        var (bodyTerm, bodyType) = Infer(body, let.Body);
        if (!performed.IsEmpty) CheckSealedStays(body, ctx.Width, body.Width, let.Name.Name, bodyType);
        return (new Term.Let(ctx.Quote(valueType), valueTerm, bodyTerm), bodyType);
    }

    private static (Term, Value) InferLam(Context ctx, Syntax.Lam lam)
    {
        var domain = lam.Param.Type is { } written ? TypeValue(ctx, written) : ctx.RawMeta();
        var since = ctx.Metas.Count;
        var inner = ctx.Bind(lam.Param.Name.Name, domain) with { Enclosing = lam.Body, HandlerScopes = [] };
        var ((body, bodyType), performed) = Collecting(inner, c => Infer(c, lam.Body));
        // A heap allocated in the body that neither the domain nor the result mentions cannot be observed.
        performed = DischargeLocalHeaps(inner, since, [domain, bodyType], performed);
        var codomain = new Closure(ctx.Environment, inner.Quote(bodyType));
        var row = RowOf(inner, performed);
        return (new Term.Lam(body), new Value.VPi(lam.Param.Explicitness, domain, codomain)
        {
            Row = row.IsPure ? RowClosure.Pure : new RowClosure(ctx.Environment, row),
        });
    }

    private static (Term, Value) InferAp(Context ctx, Syntax.Ap ap)
    {
        var (fn, fnType) = Infer(ctx, ap.Fn);
        (fn, fnType) = InsertImplicitArgs(ctx, fn, fnType);
        if (InferApWithPendingDicts(ctx, fn, fnType, ap.Arg) is { } withDicts) return withDicts;
        switch (ctx.Force(fnType))
        {
            case Value.VPi { Explicitness: Explicitness.Explicit } pi:
            {
                var (arg, argEffects) = Collecting(ctx, c => Check(c, ap.Arg, pi.Domain));
                Emit(ctx, argEffects);
                var result = Nbe.ApplyClosure(ctx.Metas, pi.Codomain, ArgumentValue(ctx, arg, argEffects));
                return (EmitLatent(ctx, pi, new Term.Ap(fn, Explicitness.Explicit, arg)), ctx.Force(result));
            }
            case Value.VMeta or Value.VVar or Value.VNeutral:
                return InferApUnknown(ctx, fn, fnType, ap.Arg);
            default:
                throw new FunException("applying non-function");
        }
    }

    /// <summary>A written type, as a term. Its own type must be a universe.</summary>
    private static Term TypeTerm(Context ctx, Syntax stx) => Pure(ctx, c => TypeOfExpr(c, stx));

    /// <summary>
    /// A written type's value. Reading a type inspects it, so a type computed by a
    /// divergent call is an evaluation budget error here rather than a deferred one.
    /// </summary>
    private static Value TypeValue(Context ctx, Syntax stx)
    {
        var value = ctx.Eval(TypeTerm(ctx, stx));
        ctx.Force(value);
        return value;
    }
}
