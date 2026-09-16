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
    /// on, that its enclosing function body or module names or its payload types
    /// mention; each payload type is closed over those captures.
    /// </summary>
    // ponytail: no generative stamp; without effects every declaration is
    // applicative (E11). Add the module stamp slot with effects.
    private static (Term, Value) InferEnum(Context ctx, Syntax.Enum e)
    {
        var payloads = e.Constructors
            .Select(c => c.Payloads.Select(p => ctx.Eval(TypeTerm(ctx, p))).ToList())
            .ToList();

        var levels = FirstBoundLevel(ctx) is int firstBound
            ? NamedLevels(ctx, ctx.Enclosing)
                .Concat(payloads.SelectMany(ps => ps.SelectMany(p => FreeLevels(ctx, p))))
                .Where(l => l >= firstBound && !ctx.RecursiveLevels.Contains(l))
                .Distinct()
                .Order()
                .ToEquatableArray()
            : [];

        EquatableArray<ConstructorDecl> constructors =
            [.. e.Constructors.Select((c, i) => new ConstructorDecl(c.Name,
                [.. payloads[i].Select(p => Unify.CloseOver(ctx.Metas, ctx.Width, levels, p))]))];
        var decl = CompletePending(ctx, e, constructors, levels) ?? new NominalDecl("enum", constructors, levels.Length);
        ctx.Metas.DeclaredNominals.Add(decl);
        var captures = levels.Select(l => (Term)new Term.Var(Nbe.LevelToIndex(ctx.Width, l))).ToEquatableArray();
        return (new Term.Nominal(decl, captures), Value.VU.Instance);
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
    private static IEnumerable<int> NamedLevels(Context ctx, Syntax? stx)
    {
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

        void Pat(Pattern p)
        {
            switch (p)
            {
                case Pattern.Prod prod: foreach (var i in prod.Items) Pat(i); break;
                case Pattern.Or o: Pat(o.Left); Pat(o.Right); break;
                case Pattern.Con c: Go(c.Head); foreach (var a in c.Args) Pat(a); break;
                case Pattern.Record r: Go(r.Type); foreach (var f in r.Fields) Pat(f.Pattern); break;
                case Pattern.StructType s: foreach (var f in s.Fields) Pat(f.Pattern); break;
            }
        }

        void Go(Syntax? s)
        {
            switch (s)
            {
                case null or Syntax.Atom or Syntax.Import: break;
                case Syntax.Var v: Name(v.Id.Name); break;
                case Syntax.OpenChoice c: Choice(c); break;
                case Syntax.Ap a: Go(a.Fn); Go(a.Arg); break;
                case Syntax.Lam l: Go(l.Param.Type); Go(l.Body); break;
                case Syntax.Let l: Go(l.Type); Go(l.Value); Go(l.Body); break;
                case Syntax.Annotated a: Go(a.Inner); Go(a.Type); break;
                case Syntax.Arrow a:
                    Go(a.Domain); Go(a.Codomain);
                    if (a.Row is { } row) foreach (var r in row.Effects.Concat(row.Tails)) Go(r);
                    break;
                case Syntax.Prod p: foreach (var i in p.Items) Go(i); break;
                case Syntax.ProdTy p: foreach (var i in p.Items) Go(i); break;
                case Syntax.Proj p: Go(p.Of); break;
                case Syntax.FieldAccess f: Go(f.Of); break;
                case Syntax.Open o: Go(o.Of); Go(o.Body); break;
                case Syntax.Match m:
                    Go(m.Scrutinee);
                    foreach (var b in m.Branches) { Pat(b.Pattern); Go(b.Body); }
                    break;
                case Syntax.Enum e: foreach (var c in e.Constructors) foreach (var p in c.Payloads) Go(p); break;
                case Syntax.PatternSynonym synonym: Pat(synonym.Rhs); break;
                case Syntax.Module m: Bindings(m.Bindings); break;
                case Syntax.Struct st: Bindings(st.Bindings); break;
                case Syntax.RecordConstruct r: Go(r.Type); foreach (var (_, v) in r.Fields) Go(v); break;
                case Syntax.LetRecGroup g: foreach (var member in g.Members) Go(member.Value); Go(g.Body); break;
                case Syntax.RefNew r: Go(r.Arg); break;
                case Syntax.RefGet r: Go(r.Ref); break;
                case Syntax.RefSet r: Go(r.Ref); Go(r.Value); break;
                default:
                    throw new NotImplementedException($"not ported yet: the names a {s.GetType().Name} uses");
            }
        }

        void Bindings(EquatableArray<Binding> bindings)
        {
            foreach (var b in bindings)
            {
                switch (b)
                {
                    case Binding.Let l: Go(l.Value); break;
                    case Binding.Open o: Go(o.Of); break;
                    case Binding.Field f: Go(f.Type); break;
                    case Binding.RecGroup g: foreach (var member in g.Members) Go(member.Value); break;
                    case Binding.Method method:
                        foreach (var p in method.Params) Go(p.Type);
                        Go(method.Body);
                        break;
                    default: throw new NotImplementedException($"not ported yet: the names a {b.GetType().Name} binding uses");
                }
            }
        }

        Go(stx);
        return found.Where(l => l < ctx.EnclosingWidth);
    }

    /// <summary>The levels below the context's width that a value's bound variables stand at.</summary>
    private static IEnumerable<int> FreeLevels(Context ctx, Value value)
    {
        var found = new List<int>();
        var mc = ctx.Metas;

        void Go(Value v, int width)
        {
            switch (Nbe.Force(mc, v))
            {
                case Value.VVar r:
                    if (r.Level < ctx.Width) found.Add(r.Level);
                    foreach (var a in r.Spine) Go(a, width);
                    break;
                case Value.VMeta m: foreach (var a in m.Spine) Go(a, width); break;
                case Value.VNeutral n:
                    if (n.Head is Head.HVar h && h.Level < ctx.Width) found.Add(h.Level);
                    foreach (var f in n.Frames) if (f is Frame.FApp app) Go(app.Arg, width);
                    break;
                case Value.VPi pi:
                    Go(pi.Domain, width);
                    Go(Nbe.ApplyClosure(mc, pi.Codomain, new Value.VVar(width, [])), width + 1);
                    break;
                case Value.VLam lam: Go(Nbe.ApplyClosure(mc, lam.Body, new Value.VVar(width, [])), width + 1); break;
                case Value.VProd p: foreach (var i in p.Items) Go(i, width); break;
                case Value.VProdTy p: foreach (var i in p.Items) Go(i, width); break;
                case Value.VNominal n: foreach (var c in n.Captures) Go(c, width); break;
                case Value.VCon c: Go(c.Nominal, width); foreach (var a in c.Args) Go(a, width); break;
                case Value.VU or Value.VAtom or Value.VAtomTy: break;
                case var other: throw new NotImplementedException($"not ported yet: the variables a {other.GetType().Name} mentions");
            }
        }

        Go(value, ctx.Width);
        return found;
    }
}
