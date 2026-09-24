using System.Collections.Immutable;
using Fun.Kernel;

namespace Fun.Compiler;

public sealed partial record Context
{
    /// <summary>
    /// The function body or module the context is elaborating inside: a nominal
    /// declared here captures what it names (E11).
    /// </summary>
    public Syntax? Enclosing
    {
        get;
        init
        {
            field = value;
            EnclosingWidth = Width;
            // The names a new enclosing module or body uses replace the last one's,
            // stamp included (as the prototype's enclosing_scope does).
            ScopeCaptures = [];
        }
    }

    /// <summary>
    /// The width where <see cref="Enclosing"/> starts. The names it uses are resolved
    /// there, as the prototype's enclosing scope is: an entry the module itself pushes
    /// later - a member, rigid when its value performs - is not captured by name.
    /// </summary>
    public int EnclosingWidth { get; private init; }

    /// <summary>
    /// The entries an <c>open</c> of a nominal pushed, by level: which constructor
    /// of which nominal each is. A constructor pattern's head is located through these.
    /// </summary>
    public ImmutableDictionary<int, (Value Type, Value TypeType, ConstructorDecl Constructor)> ConstructorEntries { get; init; } =
        ImmutableDictionary<int, (Value, Value, ConstructorDecl)>.Empty;
}

public static partial class Elaborator
{
    private const string UnportedConstructorHead =
        "not ported yet: a constructor pattern head that is a member of a non-nominal";

    /// <summary>
    /// <c>enum { … }</c>: a new declaration, whose identity is itself and the
    /// values of its captures. It captures every entry, from the first bound one
    /// on, that its enclosing function body or module names or its payload values
    /// mention - plus whatever the scope always captures, a module's stamp - and
    /// each payload type is closed over those captures.
    /// </summary>
    private static (Term, Value) InferEnum(Context ctx, Syntax.Enum e)
    {
        RejectDuplicates(e.Constructors.Select(c => c.Name), "constructor");
        var payloadTerms = e.Constructors
            .Select(c => c.Payloads.Select(p => TypeTerm(ctx, p)).ToList())
            .ToList();
        var payloads = payloadTerms.Select(ps => ps.Select(p => ctx.Eval(p)).ToList()).ToList();

        var levels = EnumCaptureLevels(ctx, payloads.SelectMany(ps => ps));

        EquatableArray<ConstructorDecl> constructors =
            [.. e.Constructors.Select((c, i) => new ConstructorDecl(c.Name,
                [.. payloads[i].Select(p => Unify.CloseOver(ctx.Metas, ctx.Width, levels, p))]))];
        var decl = CompletePending(ctx, e, constructors, levels) ?? new NominalDecl("enum", constructors, levels.Length);
        ctx.Metas.DeclaredNominals.Add(decl);
        var captures = levels.Select(l => (Term)new Term.Var(Nbe.LevelToIndex(ctx.Width, l))).ToEquatableArray();
        return (new Term.Nominal(decl, captures), Value.VU.Instance);
    }

    /// <summary>
    /// The levels an enum declared in <paramref name="ctx"/> captures: every entry,
    /// from the first bound one on, that its enclosing function body or module
    /// names or its payload values mention - each value read back as a term, so
    /// a transparent binding's entry is seen through to the variable it holds,
    /// the quotes the prototype's <c>capture_payloads</c> feeds on - plus whatever
    /// the scope always captures, a module's stamp. A recursive entry is never
    /// captured: a nominal refers to it by its declaration.
    /// </summary>
    private static EquatableArray<int> EnumCaptureLevels(Context ctx, IEnumerable<Value> payloadValues)
    {
        IEnumerable<int> mentioned = FirstBoundLevel(ctx) is int firstBound
            ? NamedLevels(ctx, ctx.Enclosing)
                .Concat(payloadValues.SelectMany(p => FreeLevels(ctx, Nbe.Quote(ctx.Metas, ctx.Width, p))))
                .Where(l => l >= firstBound && !ctx.RecursiveLevels.Contains(l))
            : Enumerable.Empty<int>();
        return mentioned.Concat(ctx.ScopeCaptures).Distinct().Order().ToEquatableArray();
    }

    /// <summary>
    /// <c>T.C</c> for a nominal type <c>T</c>, or for a type former <c>T : (A : …) -&gt; Type</c>,
    /// whose constructors are generic over its parameters
    /// (<c>Option.Some : [A : Type] -&gt; A -&gt; Option(A)</c>). Null when <c>T</c>
    /// is not a nominal type.
    /// </summary>
    private static (Term, Value)? ConstructorMember(Context ctx, Term of, Value ofType, string name)
    {
        var mc = ctx.Metas;
        var formerValue = ctx.Eval(of);
        if (PeelFormer(ctx, formerValue, ofType) is not var (nominal, domains)) return null;
        var width = ctx.Width + domains.Count;

        var constructor = nominal.Decl.Constructor(name) ?? throw new FunException($"no constructor `{name}`");
        var typeTerm = ConstructorTypeTerm(mc, width, nominal, constructor);
        for (var j = domains.Count - 1; j >= 0; j--) typeTerm = new Term.Pi(Explicitness.Implicit, domains[j], typeTerm);

        Term valueTerm = new Term.Dot(of, name);
        if (domains.Count > 0)
        {
            Term applied = Nbe.Quote(mc, width, formerValue);
            for (var j = domains.Count - 1; j >= 0; j--) applied = new Term.Ap(applied, Explicitness.Explicit, new Term.Var(j));
            valueTerm = new Term.Dot(applied, name);
            for (var j = 0; j < domains.Count; j++) valueTerm = new Term.Lam(valueTerm);
        }
        return (valueTerm, ctx.Eval(typeTerm));
    }

    /// <summary>
    /// A nominal type, or a type former applied to a bound variable per parameter:
    /// the nominal it names under those variables, and the parameters' domains as
    /// terms at the width each sits at. Null when it names no nominal.
    /// </summary>
    private static (Value.VNominal, List<Term>)? PeelFormer(Context ctx, Value value, Value type)
    {
        var (width, domains) = (ctx.Width, new List<Term>());
        type = ctx.Force(type);
        while (type is Value.VPi pi)
        {
            var arg = new Value.VVar(width, []);
            domains.Add(Nbe.Quote(ctx.Metas, width, pi.Domain));
            (value, type, width) = (Nbe.Apply(ctx.Metas, value, arg), ctx.Force(Nbe.ApplyClosure(ctx.Metas, pi.Codomain, arg)), width + 1);
        }
        return type is Value.VU && ctx.Force(value) is Value.VNominal nominal ? (nominal, domains) : null;
    }

    /// <summary>
    /// The nominal a type or type former names, a former's parameters taken as
    /// fresh metas - for a pattern, whose scrutinee solves them.
    /// </summary>
    private static Value.VNominal? Instantiate(Context ctx, Value value, Value type)
    {
        type = ctx.Force(type);
        while (type is Value.VPi pi)
        {
            var arg = ctx.RawMeta();
            (value, type) = (Nbe.Apply(ctx.Metas, value, arg), ctx.Force(Nbe.ApplyClosure(ctx.Metas, pi.Codomain, arg)));
        }
        return type is Value.VU && ctx.Force(value) is Value.VNominal nominal ? nominal : null;
    }

    /// <summary>A constructor's type, <c>P0 -&gt; … -&gt; Pn -&gt; N</c>, as a term at <paramref name="width"/> entries.</summary>
    private static Term ConstructorTypeTerm(MetaContext mc, int width, Value.VNominal nominal, ConstructorDecl constructor)
    {
        var payloads = Nbe.PayloadTypes(mc, nominal, constructor);
        Term type = Nbe.Quote(mc, width + payloads.Length, nominal);
        for (var i = payloads.Length - 1; i >= 0; i--)
            type = new Term.Pi(Explicitness.Explicit, Nbe.Quote(mc, width + i, payloads[i]), type);
        return type;
    }

    /// <summary>
    /// <c>open T</c> of a nominal type: its constructors, in declaration order,
    /// as entries marked with the constructor each is. Null when <c>T</c> is not one.
    /// </summary>
    private static (Context, Term, EquatableArray<OpenMember>)? OpenNominal(Context ctx, Term of, Value ofType, string label)
    {
        var typeValue = ctx.Eval(of);
        if (PeelFormer(ctx, typeValue, ofType) is not var (nominal, domains)) return null;

        // Each constructor as the member `T.C` would give, worked out at the
        // context the open is written in, before any entry is pushed.
        var constructors = nominal.Decl.Constructors
            .Select(c => (Constructor: c, Member: ConstructorMember(ctx, of, ofType, c.Name)!.Value))
            .Select(x => (x.Constructor, Value: ctx.Eval(x.Member.Item1), Type: x.Member.Item2))
            .ToList();

        var members = ImmutableDictionary<string, Entry>.Empty;
        var opened = new List<OpenMember>();
        foreach (var (constructor, value, type) in constructors)
        {
            (ctx, var entry) = ctx.DefineAnonymous(type, value);
            members = members.SetItem(constructor.Name, entry);
            ctx = ctx with { ConstructorEntries = ctx.ConstructorEntries.SetItem(entry.Level, (typeValue, ctx.Force(ofType), constructor)) };
            opened.Add(new OpenMember.Constructor(constructor.Name, domains.Count));
        }
        return (ctx with { Opened = ctx.Opened.SetItem(label, members) }, of, [.. opened]);
    }

    /// <summary>
    /// The nominal and constructor a pattern's head names, found through what the
    /// head resolves to - a member of the type its path names, or an entry an
    /// <c>open</c> of a nominal pushed - never by its spelling. Null otherwise.
    /// </summary>
    private static (Value.VNominal Nominal, ConstructorDecl Constructor)? ResolveConstructorHead(Context ctx, Syntax head)
    {
        switch (head)
        {
            case Syntax.FieldAccess access:
            {
                var (of, ofType) = Infer(ctx, access.Of);
                // `M.C`, where `C` is a constructor exported into the module `M`.
                if (ctx.Force(ofType) is Value.VModule module && module.PublicMember(access.Field) is { Constructor: { } mark })
                    return Instantiate(ctx, mark.Type, mark.TypeType) is { } exported ? (exported, mark.Constructor) : null;
                if (Instantiate(ctx, ctx.Eval(of), ofType) is not { } nominal) return null;
                var constructor = nominal.Decl.Constructor(access.Field) ?? throw new FunException($"unknown constructor `{access.Field}`");
                return (nominal, constructor);
            }

            // A bare head resolves like any other name - through its binder or an
            // open - never by name among the scrutinee's constructors (decided
            // 2026-09-16). A raw `enum`'s constructors are therefore in scope only
            // through `open`; `type` writes that open. Resolving to anything but a
            // constructor is an error.
            case Syntax.Var v:
                return ConstructorEntryAt(ctx, ctx.Locate(v.Id.Name).Index)
                    ?? throw new FunException($"`{Label(v.Id.Name)}` is not a constructor in scope");

            case Syntax.OpenChoice choice:
                return ConstructorEntryAt(ctx, ctx.LocateChoice(choice.Name.Name, choice.Opens, choice.Fallback).Index)
                    ?? throw new FunException($"`{choice.Name.Name}` is not a constructor in scope");

            default:
                return null;
        }
    }

    private static (Value.VNominal, ConstructorDecl)? ConstructorEntryAt(Context ctx, int index) =>
        ctx.ConstructorEntries.TryGetValue(ctx.Width - 1 - index, out var found)
            && Instantiate(ctx, found.Type, found.TypeType) is { } nominal
            ? (nominal, found.Constructor)
            : null;

    private static int? FirstBoundLevel(Context ctx)
    {
        for (var i = ctx.EntryKinds.Length - 1; i >= 0; i--)
            if (ctx.EntryKinds[i] == EntryKind.Bound)
                return ctx.Width - 1 - i;
        return null;
    }

    /// <summary>
    /// The levels of the context's entries that the names written in <paramref name="stx"/>
    /// locate, among those that exist where the enclosing module or body starts.
    /// </summary>
    // One structural traversal, Syntax.Map: a form the walk does not know cannot
    // hide a name it uses. The prototype's enclosing_scope reads the same mapper.
    private static IEnumerable<int> NamedLevels(Context ctx, Syntax? stx)
    {
        if (stx is null) return [];
        var found = new List<int>();

        void Name(string name)
        {
            if (ctx.Names.TryGetValue(name, out var entry)) found.Add(entry.Level);
        }

        void Choice(Syntax.OpenChoice c)
        {
            foreach (var label in c.Opens)
                if (ctx.Opened.TryGetValue(label, out var members) && members.TryGetValue(c.Name.Name, out var member))
                {
                    found.Add(member.Level);
                    return;
                }
            if (c.Fallback is not null) Name(c.Fallback);
        }

        stx.Map(new SyntaxMapper
        {
            Id = id =>
            {
                Name(id.Name);
                return id;
            },
            Form = form =>
            {
                if (form is Syntax.OpenChoice c) Choice(c);
                return form;
            },
        });
        return found.Where(l => l < ctx.EnclosingWidth);
    }

    /// <summary>
    /// The levels below the context's width that a payload term's bound variables stand at.
    /// </summary>
    // The one core-term traversal, Term.Map: a form it did not walk could hide a
    // captured variable (the prototype's capture_payloads reads map_subterms).
    private static IEnumerable<int> FreeLevels(Context ctx, Term term)
    {
        var found = new List<int>();
        term.Map((t, under) =>
        {
            if (t is Term.Var v && v.Index >= under)
                found.Add(ctx.Width - 1 - (v.Index - under));
            return null;
        });
        return found;
    }
}
