using Fun.Kernel;

namespace Fun.Compiler;

/// <summary>
/// Reflection: <see cref="Syntax"/> and the reflection types of the prelude's
/// <c>Syntax</c> module are one grammar seen twice. <c>Reflect*</c> builds the value
/// of a form and <c>Read*</c> reads a form back; reading what was reflected is the
/// identity on every field (M1). A value that is not well-formed reflection reads
/// back as null, never as a guess. A reflected form the port deliberately has no
/// image for is a language error naming the rule, never an unported path.
/// </summary>
// Not carried, because the reflection grammar has no slot for them: the spans of
// a path's members, and the roles an open's region holds (expansion recomputes
// them when it reaches the open again).
public sealed class Reflection
{
    private readonly MetaContext _metas;
    private readonly Value _stdlib;

    private Reflection(MetaContext metas, Value stdlib)
    {
        _metas = metas;
        _stdlib = stdlib;
        ExprType = Nominal(PreludeAbi.Syntax, PreludeAbi.Types.Syntax.Expr);
        DeclType = Nominal(PreludeAbi.Syntax, PreludeAbi.Types.Syntax.Decl);
        PatternType = Nominal(PreludeAbi.Syntax, PreludeAbi.Types.Syntax.Pattern);
        TokenTreeType = Nominal(PreludeAbi.Syntax, PreludeAbi.Types.Syntax.TokenTree);
        RType = Nominal(PreludeAbi.Syntax, PreludeAbi.Types.Syntax.R);
        _atomVal = Nominal(PreludeAbi.Syntax, PreludeAbi.Types.Syntax.AtomVal);
        _tokenKind = Nominal(PreludeAbi.Syntax, PreludeAbi.Types.Syntax.TokenKind);
        _role = Nominal(PreludeAbi.Syntax, PreludeAbi.Types.Syntax.Role);
        _order = Nominal(PreludeAbi.Syntax, PreludeAbi.Types.Syntax.Order);
        _roleMeaning = Nominal(PreludeAbi.Syntax, PreludeAbi.Types.Syntax.RoleMeaning);
        _rule = Nominal(PreludeAbi.Syntax, PreludeAbi.Types.Syntax.Rule);
        _rulePart = Nominal(PreludeAbi.Syntax, PreludeAbi.Types.Syntax.RulePart);
        _replacement = Nominal(PreludeAbi.Syntax, PreludeAbi.Types.Syntax.Replacement);
        _capture = Nominal(PreludeAbi.Syntax, PreludeAbi.Types.Syntax.Capture);
        _captured = Nominal(PreludeAbi.Syntax, PreludeAbi.Types.Syntax.Captured);
        _field = Nominal(PreludeAbi.Syntax, PreludeAbi.Types.Syntax.Field);
        _quoteHole = Nominal(PreludeAbi.Syntax, PreludeAbi.Types.Syntax.QuoteHole);
        _param = Nominal(PreludeAbi.Syntax, PreludeAbi.Types.Syntax.Param);
        _effectRow = Nominal(PreludeAbi.Syntax, PreludeAbi.Types.Syntax.EffectRow);
        _effectOp = Nominal(PreludeAbi.Syntax, PreludeAbi.Types.Syntax.EffectOp);
        _ctor = Nominal(PreludeAbi.Syntax, PreludeAbi.Types.Syntax.Ctor);
        _branch = Nominal(PreludeAbi.Syntax, PreludeAbi.Types.Syntax.Branch);
        _patField = Nominal(PreludeAbi.Syntax, PreludeAbi.Types.Syntax.PatField);

        // The shapes the compiler builds are probed off the builders the prelude
        // publishes for them, so no constructor, field or leaf tag is spelled here.
        _bool = LeafsOf(Builder(PreludeAbi.Builders.Builtins.I64ToBool), 2);
        // Each probe leads with the builder's own type binder, which the term's
        // lambda chain counts like any other (Nbe.Apply knows no explicitness).
        _some = CtorOf(Builder(PreludeAbi.Builders.Builtins.MkOption), Probe, I64(0), Probe);
        _none = CtorOf(Builder(PreludeAbi.Builders.Builtins.MkOption), Probe, I64(1), Probe);
        _nil = CtorOf(Builder(PreludeAbi.Builders.Builtins.MkList), Probe, I64(0), Probe, Probe);
        _cons = CtorOf(Builder(PreludeAbi.Builders.Builtins.MkList), Probe, I64(1), Probe, Probe);
        _explicitness = LeafsOf(SyntaxBuilder(PreludeAbi.Builders.Syntax.Explicitness), 2);
        _fixity = LeafsOf(SyntaxBuilder(PreludeAbi.Builders.Syntax.Fixity), 2);
        _delim = LeafsOf(SyntaxBuilder(PreludeAbi.Builders.Syntax.Delim), 3);
        _assoc = LeafsOf(SyntaxBuilder(PreludeAbi.Builders.Syntax.Assoc), 3);
        _holeKind = LeafsOf(SyntaxBuilder(PreludeAbi.Builders.Syntax.HoleKind), 7);
        _atomTy = LeafsOf(SyntaxBuilder(PreludeAbi.Builders.Syntax.AtomTy), 6);
        _macroAnn = LeafsOf(SyntaxBuilder(PreludeAbi.Builders.Syntax.MacroAnn), 2);
        _span = LayoutOf(SyntaxBuilder(PreludeAbi.Builders.Syntax.MkSpan), 7);
        _id = LayoutOf(SyntaxBuilder(PreludeAbi.Builders.Syntax.MkId), 3);
        _pathChoice = LayoutOf(SyntaxBuilder(PreludeAbi.Builders.Syntax.MkPathChoice), 2);
        _path = LayoutOf(SyntaxBuilder(PreludeAbi.Builders.Syntax.MkPath), 3);
        _patWild = CtorOf(SyntaxBuilder(PreludeAbi.Builders.Syntax.PatWild));
        _patBind = CtorOf(SyntaxBuilder(PreludeAbi.Builders.Syntax.PatVar), Probe);
        _patAtom = CtorOf(SyntaxBuilder(PreludeAbi.Builders.Syntax.PatAtom), Probe);
        _patProd = CtorOf(SyntaxBuilder(PreludeAbi.Builders.Syntax.PatProd), Probe);
        _patOr = CtorOf(SyntaxBuilder(PreludeAbi.Builders.Syntax.PatOr), Probe, Probe);
        DeclsType = Nbe.Force(_metas, Member(PreludeAbi.Syntax, PreludeAbi.Types.Syntax.Decls));
        IdType = _id.Type;
    }

    private static readonly Lazy<Reflection> PreludeReflection =
        new(() => new Reflection(Prelude.SyntaxStage.Metas, Prelude.SyntaxStage.Value));

    /// <summary>Reflection over the prelude's <c>Syntax</c> module.</summary>
    public static Reflection OfPrelude => PreludeReflection.Value;

    public Value.VNominal ExprType { get; }
    public Value.VNominal DeclType { get; }
    public Value.VNominal PatternType { get; }
    public Value.VNominal TokenTreeType { get; }

    /// <summary><c>Syntax.R</c>: a type binder's reflected solution (<c>RExpr(T)</c>, <c>RDecls</c>, <c>RPat</c>).</summary>
    public Value.VNominal RType { get; }

    /// <summary><c>Syntax.Decls</c>, the type of a declaration list: <c>List(Decl)</c>.</summary>
    public Value DeclsType { get; }

    /// <summary><c>Syntax.Id</c>, the record type of an identifier.</summary>
    public Value IdType { get; }

    private readonly Value.VNominal _atomVal, _tokenKind, _role, _order, _roleMeaning, _rule, _rulePart,
        _replacement, _capture, _captured, _field, _quoteHole, _param, _effectRow, _effectOp, _ctor, _branch, _patField;

    private readonly Leafs _bool, _explicitness, _fixity, _delim, _assoc, _holeKind, _atomTy, _macroAnn;
    private readonly Ctor _some, _none, _nil, _cons, _patWild, _patBind, _patAtom, _patProd, _patOr;
    private readonly Layout _span, _id, _pathChoice, _path;

    // ---- reaching the prelude's published builders, once ----------------------

    /// <summary>An argument no builder inspects: probe shapes, never values.</summary>
    private static readonly Value Probe = Value.VU.Instance;

    private static Value I64(long n) => new Value.VAtom(new Atom.I64(n));

    private Value Builder(string name) => Member(name);

    private Value SyntaxBuilder(string name) => Member(PreludeAbi.Syntax, name);

    private Value Applied(Value builder, params Value[] args) =>
        args.Aggregate(builder, (fn, arg) => Nbe.Apply(_metas, fn, arg));

    /// <summary>A constructor, resolved by building it once from a published builder.</summary>
    private Ctor CtorOf(Value builder, params Value[] probe) =>
        Nbe.Force(_metas, Applied(builder, probe)) is Value.VCon c
            ? new Ctor(c.Name, c.Nominal, c.Args.Length)
            : throw new InvalidOperationException("a prelude builder did not build a constructor");

    /// <summary>A struct, resolved by building it once from a published builder.</summary>
    private Layout LayoutOf(Value builder, int arity) =>
        Nbe.Force(_metas, Applied(builder, [.. Enumerable.Range(0, arity).Select(_ => Probe)])) is Value.VRecord r
            ? new Layout(r.Type, [.. r.Fields.Select(f => f.Name)])
            : throw new InvalidOperationException("a prelude builder did not build a record");

    /// <summary>A leaf enum's constructors in code order, resolved by building each code once.</summary>
    private Leafs LeafsOf(Value builder, int count) =>
        new([.. Enumerable.Range(0, count).Select(code => Nbe.Force(_metas, Applied(builder, I64(code))))]);

    /// <summary>A constructor the prelude builds: its tag, nominal and arity, probed, never spelled here.</summary>
    private sealed record Ctor(string Tag, Value.VNominal Nominal, int Arity)
    {
        /// <summary>Build a constructor value; the arity the probe saw must hold.</summary>
        public Value Build(params Value[] args) => args.Length == Arity
            ? new Value.VCon(Tag, [.. args], Nominal)
            : throw new InvalidOperationException($"the prelude's {Tag} takes {Arity} payloads, not {args.Length}");
    }

    /// <summary>A struct the prelude builds: its type and field names, in order, probed.</summary>
    private sealed record Layout(Value Type, EquatableArray<string> Fields)
    {
        public Value Build(params Value[] values) => new Value.VRecord(Type, [.. Fields.Zip(values, (name, value) => (name, value))]);
    }

    /// <summary>A leaf enum's constructors in C# code order, probed: the code reads the tag back.</summary>
    private sealed record Leafs(EquatableArray<Value> Values)
    {
        public Value.VNominal Nominal => ((Value.VCon)Values[0]).Nominal;

        public Value At(int code) => Values[code];

        public int CodeOf(string tag)
        {
            for (var i = 0; i < Values.Length; i++) if (Values[i] is Value.VCon { Args.IsEmpty: true } c && c.Name == tag) return i;
            return -1;
        }
    }

    private Value Member(params string[] path) =>
        path.Aggregate(_stdlib, (acc, name) => Nbe.DotValue(Nbe.Force(_metas, acc), name));

    /// <summary>
    /// A compiler-known type's nominal: a parameterised type's name is its former,
    /// applied to placeholders until the nominal appears. Reflected values are told
    /// apart by their declaration, never by the arguments.
    /// </summary>
    private Value.VNominal Nominal(params string[] path)
    {
        var v = Nbe.Force(_metas, Member(path));
        while (v is Value.VLam) v = Nbe.Force(_metas, Nbe.Apply(_metas, v, Probe));
        return v as Value.VNominal ?? throw new InvalidOperationException($"the prelude's {string.Join(".", path)} is not a nominal type");
    }

    // ---- building values -----------------------------------------------------

    private static Value Con(Value.VNominal nominal, string name, params Value[] args) => new Value.VCon(name, [.. args], nominal);

    private static Value Str(string s) => new Value.VAtom(new Atom.Str(s));

    private Value Bool(bool b) => _bool.At(b ? 1 : 0);

    private Value Option<T>(T? x, Func<T, Value> f) where T : class =>
        x is null ? _none.Build() : _some.Build(f(x));

    private Value OptionOf(Value? x) => x is null ? _none.Build() : _some.Build(x);

    private Value List<T>(IEnumerable<T> items, Func<T, Value> f) =>
        items.Reverse().Aggregate(_nil.Build(), (tail, head) => _cons.Build(f(head), tail));

    /// <summary>A resolved name carries the certificate that it was minted as one (M12).</summary>
    private static string? Certificate(string name) => name.Contains('#') ? name : null;

    private static bool Certified(string name, string? certificate) => !name.Contains('#') || certificate == name;

    private Value Span(SourceSpan span) => span.IsSynthetic
        ? _none.Build()
        : _some.Build(_span.Build(
            Option(span.File, Str),
            I64(span.Start),
            I64(span.End),
            OptionOf(span.StartLine is int sl ? I64(sl) : null),
            OptionOf(span.StartCol is int sc ? I64(sc) : null),
            OptionOf(span.EndLine is int el ? I64(el) : null),
            OptionOf(span.EndCol is int ec ? I64(ec) : null)));

    public Value ReflectId(Id id) => _id.Build(
        Str(id.Name),
        Span(id.Span),
        new Value.VAtom(new Atom.Scopes(id.Scope, Certificate(id.Name))));

    private Value Explicitness(Explicitness e) => _explicitness.At((int)e);

    private Value AtomVal(Atom a) => a switch
    {
        Atom.I64 n => Con(_atomVal, PreludeAbi.Tags.AtomVal.I64Atom, new Value.VAtom(n)),
        Atom.Char c => Con(_atomVal, PreludeAbi.Tags.AtomVal.CharAtom, new Value.VAtom(c)),
        Atom.Str s => Con(_atomVal, PreludeAbi.Tags.AtomVal.StringAtom, new Value.VAtom(s)),
        Atom.Unit => Con(_atomVal, PreludeAbi.Tags.AtomVal.UnitAtom),
        Atom.Scopes s => Con(_atomVal, PreludeAbi.Tags.AtomVal.ScopesAtom, new Value.VAtom(s)),
        _ => throw new InvalidOperationException($"unhandled atom {a.GetType().Name}"),
    };

    private Value AtomTyVal(AtomTy t) => _atomTy.At((int)t);

    /// <summary>A path form -- a name, an open choice, a member of one -- as a <c>Syntax.Path</c>.</summary>
    private Value Path(Syntax form)
    {
        var members = new List<string>();
        while (form is Syntax.FieldAccess f)
        {
            members.Insert(0, f.Field);
            form = f.Of;
        }
        var (head, choice) = form switch
        {
            Syntax.Var v => (v.Id, (Value?)null),
            Syntax.OpenChoice c => (c.Name, (Value?)_pathChoice.Build(List(c.Opens, Str), Option(c.Fallback, Str))),
            _ => throw new InvalidOperationException($"unhandled path form {form.GetType().Name}"),
        };
        return _path.Build(ReflectId(head), List(members, Str), OptionOf(choice));
    }

    private Value Ann(FormKind k) => _macroAnn.At((int)k);
    private Value FixityVal(Fixity f) => _fixity.At((int)f);

    private Value Delim(Delimiter d) => _delim.At((int)d);

    private Value TokenKindVal(TokenKind k) => k switch
    {
        TokenKind.Ident i => Con(_tokenKind, PreludeAbi.Tags.TokenKind.IdentTok, Str(i.Name)),
        TokenKind.Operator o => Con(_tokenKind, PreludeAbi.Tags.TokenKind.OperatorTok, Str(o.Spelling)),
        TokenKind.Int n => Con(_tokenKind, PreludeAbi.Tags.TokenKind.IntTok, I64(n.Value)),
        TokenKind.Char c => Con(_tokenKind, PreludeAbi.Tags.TokenKind.CharTok, new Value.VAtom(new Atom.Char(c.Value))),
        TokenKind.Str s => Con(_tokenKind, PreludeAbi.Tags.TokenKind.StringTok, Str(s.Value)),
        TokenKind.Word w when TokenKind.Keywords.ContainsKey(w.Spelling) => Con(_tokenKind, PreludeAbi.Tags.TokenKind.KeywordTok, Str(w.Spelling)),
        TokenKind.Word w => Con(_tokenKind, PreludeAbi.Tags.TokenKind.PunctTok, Str(w.Spelling)),
        _ => throw new InvalidOperationException($"unhandled token kind {k.GetType().Name}"),
    };

    /// <summary>A token tree, each token with its scope set (M9).</summary>
    public Value ReflectTokenTree(Fun.Kernel.TokenTree t) => t switch
    {
        Fun.Kernel.TokenTree.Leaf { Token: var tok } => Con(TokenTreeType, PreludeAbi.Tags.TokenTree.Tok, Span(tok.Span), TokenKindVal(tok.Kind),
            new Value.VAtom(new Atom.Scopes(tok.Scope, tok.Kind is TokenKind.Ident i ? Certificate(i.Name) : null))),
        Fun.Kernel.TokenTree.Group g => Con(TokenTreeType, PreludeAbi.Tags.TokenTree.TokGroup, Span(g.Span), Delim(g.Delimiter), List(g.Items, ReflectTokenTree)),
        _ => throw new InvalidOperationException($"unhandled token tree {t.GetType().Name}"),
    };

    public Value ReflectTokens(IEnumerable<Fun.Kernel.TokenTree> ts) => List(ts, ReflectTokenTree);

    private Value AssocVal(Assoc a) => _assoc.At((int)a);

    private Value HoleKindVal(HoleKind k) => _holeKind.At((int)k);

    public Value ReflectExpr(Syntax stx)
    {
        Value E(string name, params Value[] args) => Con(ExprType, name, [Span(stx.Span), .. args]);
        Value X(Syntax s) => ReflectExpr(s);
        Value? XOpt(Syntax? s) => s is null ? null : ReflectExpr(s);

        return stx switch
        {
            Syntax.Var v => E(PreludeAbi.Tags.Expr.RawVar, ReflectId(v.Id)),
            Syntax.Atom a => E(PreludeAbi.Tags.Expr.RawAtom, AtomVal(a.Value)),
            Syntax.Self => E(PreludeAbi.Tags.Expr.RawSelf),
            Syntax.SelfType => E(PreludeAbi.Tags.Expr.RawSelfType),
            Syntax.Ap a => E(PreludeAbi.Tags.Expr.RawAp, X(a.Fn), Explicitness(a.Explicitness), X(a.Arg)),
            Syntax.Lam l => E(PreludeAbi.Tags.Expr.RawLam, ParamVal(l.Param), X(l.Body)),
            Syntax.Let l => E(PreludeAbi.Tags.Expr.RawLet, ReflectId(l.Name), OptionOf(XOpt(l.Type)), X(l.Value), X(l.Body), Bool(l.Recursive)),
            Syntax.LetRecGroup g => E(PreludeAbi.Tags.Expr.RawLetRecGroup, List(g.Members, m => ReflectId(m.Name)), List(g.Members, m => X(m.Value)), X(g.Body)),
            Syntax.Annotated a => E(PreludeAbi.Tags.Expr.RawAnnotated, X(a.Inner), X(a.Type)),
            Syntax.Prod p => E(PreludeAbi.Tags.Expr.RawProd, List(p.Items, X)),
            Syntax.ProdTy p => E(PreludeAbi.Tags.Expr.RawProdTy, List(p.Items, X)),
            Syntax.TraitBoundSet b => E(PreludeAbi.Tags.Expr.RawTraitBoundSet, List(b.Traits, X)),
            Syntax.Arrow a => E(PreludeAbi.Tags.Expr.RawArrow, Explicitness(a.Explicitness), Option(a.Name, ReflectId), X(a.Domain), Option(a.Row, EffectRowVal), X(a.Codomain)),
            Syntax.FieldAccess f => E(PreludeAbi.Tags.Expr.RawFieldAccess, X(f.Of), Str(f.Field)),
            Syntax.Proj p => E(PreludeAbi.Tags.Expr.RawProj, X(p.Of), I64(p.Index)),
            Syntax.RecordConstruct r => E(PreludeAbi.Tags.Expr.RawRecordConstruct, X(r.Type), Fields(r.Fields)),
            Syntax.Struct s => E(PreludeAbi.Tags.Expr.RawStruct, List(s.Bindings, ReflectDecl)),
            Syntax.Module m => E(PreludeAbi.Tags.Expr.RawModule, List(m.Bindings, ReflectDecl)),
            Syntax.Sig s => E(PreludeAbi.Tags.Expr.RawSig, List(s.Bindings, ReflectDecl)),
            Syntax.Enum e => E(PreludeAbi.Tags.Expr.RawEnum, _none.Build(),
                List(e.Constructors, c => Con(_ctor, PreludeAbi.Tags.Ctor.MkCtor, ReflectId(new Id(c.Name, SourceSpan.Synthetic)), List(c.Payloads, X)))),
            Syntax.Import i => E(PreludeAbi.Tags.Expr.RawImport, Str(i.Path), new Value.VAtom(new Atom.Scopes(i.Scope, null))),
            Syntax.Open o => E(PreludeAbi.Tags.Expr.RawOpen, X(o.Of), X(o.Body), Str(o.Label)),
            Syntax.OpenChoice c => E(PreludeAbi.Tags.Expr.RawOpenChoice, ReflectId(c.Name), List(c.Opens, Str), Option(c.Fallback, Str)),
            Syntax.EffectDef d => E(PreludeAbi.Tags.Expr.RawEffectDef, ReflectId(d.Name), List(d.Params, ReflectId), List(d.Ops, EffectOpVal), X(d.Body)),
            Syntax.TraitDef t => E(PreludeAbi.Tags.Expr.RawTraitDef, ReflectId(t.Name), List([t.Param], ReflectId), Fields(t.Fields), X(t.Body)),
            Syntax.ImplDef i => E(PreludeAbi.Tags.Expr.RawImplDef, Option(i.Name, ReflectId), Path(i.TraitPath), List([i.Arg], X), Fields(i.Fields), X(i.Body)),
            Syntax.Perform p => E(PreludeAbi.Tags.Expr.RawPerform, Path(p.Operation), X(p.Arg)),
            Syntax.Resume r => E(PreludeAbi.Tags.Expr.RawResume, X(r.Arg)),
            Syntax.RefNew n => E(PreludeAbi.Tags.Expr.RawRefNew, X(n.Arg)),
            Syntax.RefGet g => E(PreludeAbi.Tags.Expr.RawRefGet, X(g.Ref)),
            Syntax.RefSet s => E(PreludeAbi.Tags.Expr.RawRefSet, X(s.Ref), X(s.Value)),
            Syntax.Match m => E(PreludeAbi.Tags.Expr.RawMatch, X(m.Scrutinee), List(m.Branches, BranchVal)),
            Syntax.Stx s => E(PreludeAbi.Tags.Expr.RawStx, X(s.Inner)),
            // A macro sees the argument, not the elaborator's note that it is done.
            Syntax.Elaborated el => X(el.Form),
            Syntax.Quote q => E(PreludeAbi.Tags.Expr.RawQuote, X(q.Template), QuoteHoles(q.Holes)),
            Syntax.QuoteDecls q => E(PreludeAbi.Tags.Expr.RawQuoteDecls, List(q.Items, ReflectDecl), QuoteHoles(q.Holes)),
            Syntax.MacroDef d => E(PreludeAbi.Tags.Expr.RawMacroDef, ReflectId(d.Name), X(d.Value), X(d.Body), OptionOf(d.Kind is { } k ? Ann(k) : null), OptionOf(XOpt(d.Output))),
            Syntax.SyntaxDef d => E(PreludeAbi.Tags.Expr.RawSyntaxDef, ReflectId(d.Name), RoleVal(d.Role), X(d.Body)),
            Syntax.Block b => E(PreludeAbi.Tags.Expr.RawBlock, ReflectTokens(b.Terms)),
            Syntax.Instantiate i => E(PreludeAbi.Tags.Expr.RawInstantiate, ReflectId(i.Instantiation.Form), RuleVal(i.Instantiation.Rule),
                Captures(i.Instantiation.Captures), Option(i.Instantiation.FromUnit, Str)),
            Syntax.MacroCall c => E(PreludeAbi.Tags.Expr.RawMacroCall, X(c.Head), List(c.Args, ReflectCaptured)),
            Syntax.OperatorUse u => E(PreludeAbi.Tags.Expr.RawOperatorUse, ReflectId(u.Operator), FixityVal(u.Fixity), List(u.Operands, X),
                Span(u.DeclaredAt), Span(u.Span), Option(u.FromUnit, Str)),
            _ => throw new InvalidOperationException($"unhandled syntax form {stx.GetType().Name}"),
        };
    }

    private Value RoleVal(Role r)
    {
        var meaning = r.Meaning switch
        {
            RoleMeaning.ApplyValue => Con(_roleMeaning, PreludeAbi.Tags.RoleMeaning.ApplyValue),
            RoleMeaning.AssignRef => Con(_roleMeaning, PreludeAbi.Tags.RoleMeaning.AssignRef),
            RoleMeaning.CallMacro => Con(_roleMeaning, PreludeAbi.Tags.RoleMeaning.CallMacro),
            RoleMeaning.Rules rules => Con(_roleMeaning, PreludeAbi.Tags.RoleMeaning.Rules, Ann(rules.Kind), List(rules.Items, RuleVal)),
            RoleMeaning.OrderGroup => Con(_roleMeaning, PreludeAbi.Tags.RoleMeaning.OrderGroup),
            RoleMeaning.PolyArrow => Con(_roleMeaning, PreludeAbi.Tags.RoleMeaning.PolyArrow),
            _ => throw new InvalidOperationException($"unhandled role meaning {r.Meaning.GetType().Name}"),
        };
        return Con(_role, PreludeAbi.Tags.Role.MkRole, FixityVal(r.Fixity), Option(r.Order, OrderVal), meaning, Span(r.DeclaredAt), Option(r.FromUnit, Str));
    }

    private Value OrderVal(Order o) => Con(_order, PreludeAbi.Tags.Order.MkOrder, Str(o.Group), Str(o.Name), AssocVal(o.Assoc), Bool(o.Weakest),
        List(o.StrongerThan, OrderVal), List(o.WeakerThan, OrderVal));

    private Value RuleVal(Rule r) => Con(_rule, PreludeAbi.Tags.Rule.MkRule, List(r.Pattern, RulePartVal),
        r.Replacement switch
        {
            Replacement.Expr e => Con(_replacement, PreludeAbi.Tags.Replacement.ReplaceExpr, ReflectExpr(e.Syntax)),
            Replacement.Decls d => Con(_replacement, PreludeAbi.Tags.Replacement.ReplaceDecls, List(d.Bindings, ReflectDecl)),
            _ => throw new InvalidOperationException($"unhandled replacement {r.Replacement.GetType().Name}"),
        },
        Span(r.Span));

    private Value RulePartVal(RulePart p) => p switch
    {
        RulePart.Literal l => Con(_rulePart, PreludeAbi.Tags.RulePart.PartToken, ReflectTokenTree(l.Term)),
        RulePart.Group g => Con(_rulePart, PreludeAbi.Tags.RulePart.PartGroup, Delim(g.Delimiter), List(g.Parts, RulePartVal), Span(g.Span)),
        RulePart.Hole h => Con(_rulePart, PreludeAbi.Tags.RulePart.PartHole, Str(h.Name), HoleKindVal(h.Kind), Span(h.Span)),
        _ => throw new InvalidOperationException($"unhandled rule part {p.GetType().Name}"),
    };

    private Value ReflectCaptured(Capture c) => c switch
    {
        Capture.Expr e => Con(_captured, PreludeAbi.Tags.Captured.CapExpr, ReflectExpr(e.Syntax)),
        Capture.Block b => Con(_captured, PreludeAbi.Tags.Captured.CapBlock, ReflectTokens(b.Terms)),
        Capture.Id i => Con(_captured, PreludeAbi.Tags.Captured.CapId, ReflectTokenTree(new Fun.Kernel.TokenTree.Leaf(i.Token))),
        Capture.Pattern p => Con(_captured, PreludeAbi.Tags.Captured.CapPattern, ReflectPattern(p.Value)),
        Capture.Decls d => Con(_captured, PreludeAbi.Tags.Captured.CapDecls, List(d.Bindings, ReflectDecl)),
        Capture.Decl d => Con(_captured, PreludeAbi.Tags.Captured.CapDecl, ReflectDecl(d.Binding)),
        Capture.Tokens t => Con(_captured, PreludeAbi.Tags.Captured.CapTokens, ReflectTokens(t.Terms)),
        _ => throw new InvalidOperationException($"unhandled capture {c.GetType().Name}"),
    };

    private Value Captures(EquatableArray<(string Hole, Capture Capture)> captures) =>
        List(captures, c => Con(_capture, PreludeAbi.Tags.Capture.MkCapture, Str(c.Hole), ReflectCaptured(c.Capture)));

    private Value QuoteHoles(EquatableArray<(string Hole, Syntax Value)> holes) =>
        List(holes, h => Con(_quoteHole, PreludeAbi.Tags.QuoteHole.MkQuoteHole, Str(h.Hole), ReflectExpr(h.Value)));

    private Value Fields(EquatableArray<(string Name, Syntax Value)> fields) =>
        List(fields, f => Con(_field, PreludeAbi.Tags.Field.MkField, Str(f.Name), ReflectExpr(f.Value)));

    // A source binder's trait bounds are written in its type (a TraitBoundSet); the
    // reflected bound paths carry what a macro put there.
    private Value ParamVal(Param p) =>
        Con(_param, PreludeAbi.Tags.Param.MkParam, ReflectId(p.Name), OptionOf(p.Type is null ? null : ReflectExpr(p.Type)),
            List(p.Bounds, Path), Explicitness(p.Explicitness));

    private Value EffectRowVal(EffectRow row) =>
        Con(_effectRow, PreludeAbi.Tags.EffectRow.MkEffectRow, List(row.Effects, ReflectExpr), List(row.Tails, ReflectExpr), Bool(row.Inferred), Bool(row.Polymorphic));

    private Value EffectOpVal(EffectOp op) => Con(_effectOp, PreludeAbi.Tags.EffectOp.MkEffectOp, Str(op.Name), ReflectExpr(op.Input), ReflectExpr(op.Output));

    private Value BranchVal(MatchBranch b) => b.Operation is { } op
        ? Con(_branch, PreludeAbi.Tags.Branch.EffectBranch, Path(op), ReflectPattern(b.Pattern), ReflectExpr(b.Body))
        : Con(_branch, PreludeAbi.Tags.Branch.ValueBranch, ReflectPattern(b.Pattern), ReflectExpr(b.Body));

    public Value ReflectPattern(Fun.Kernel.Pattern p)
    {
        // Patterns carry no span: the reflected span is always None.
        Value P(string name, params Value[] args) => Con(PatternType, name, [_none.Build(), .. args]);
        Value PatField((string Name, Fun.Kernel.Pattern Pattern) f) => Con(_patField, PreludeAbi.Tags.PatField.MkPatField, Str(f.Name), _some.Build(ReflectPattern(f.Pattern)));
        return p switch
        {
            Fun.Kernel.Pattern.Wild => _patWild.Build(_none.Build()),
            Fun.Kernel.Pattern.Bind b => _patBind.Build(_none.Build(), ReflectId(b.Name)),
            Fun.Kernel.Pattern.Con c => P(PreludeAbi.Tags.Pattern.RawPatCon, Path(c.Head), List(c.Args, ReflectPattern)),
            Fun.Kernel.Pattern.Atom a => _patAtom.Build(_none.Build(), AtomVal(a.Value)),
            Fun.Kernel.Pattern.Prod pr => _patProd.Build(_none.Build(), List(pr.Items, ReflectPattern)),
            Fun.Kernel.Pattern.Or o => _patOr.Build(_none.Build(), ReflectPattern(o.Left), ReflectPattern(o.Right)),
            Fun.Kernel.Pattern.Record r => P(PreludeAbi.Tags.Pattern.RawPatRecord, Path(r.Type), List(r.Fields, PatField), Bool(r.Partial)),
            Fun.Kernel.Pattern.StructType s => P(PreludeAbi.Tags.Pattern.RawPatStructType, List(s.Fields, PatField), Bool(s.Partial)),
            Fun.Kernel.Pattern.AtomType t => P(PreludeAbi.Tags.Pattern.RawPatType, AtomTyVal(t.Ty)),
            _ => throw new InvalidOperationException($"unhandled pattern {p.GetType().Name}"),
        };
    }

    public Value ReflectDecl(Binding b)
    {
        Value D(string name, params Value[] args) => Con(DeclType, name, args);
        return b switch
        {
            Binding.Let { Value: Syntax.PatternSynonym s } l => D(PreludeAbi.Tags.Decl.DeclPatternSyn, ReflectId(l.Name), List(s.Params, ReflectId), ReflectPattern(s.Rhs), Bool(l.Public)),
            Binding.Let l => D(PreludeAbi.Tags.Decl.DeclLet, ReflectId(l.Name), ReflectExpr(l.Value), Bool(l.Public), Bool(l.Recursive)),
            Binding.RecGroup g => D(PreludeAbi.Tags.Decl.DeclRecGroup, List(g.Members, m => ReflectId(m.Name)), List(g.Members, m => ReflectExpr(m.Value)), Bool(g.Public)),
            Binding.Method m => D(PreludeAbi.Tags.Decl.DeclMethod, ReflectId(m.Name), List(m.Params, ParamVal), Option(m.Row, EffectRowVal), ReflectExpr(m.Body), Bool(m.Public)),
            Binding.Effect e => D(PreludeAbi.Tags.Decl.DeclEffect, ReflectId(e.Name), List(e.Params, ReflectId), List(e.Ops, EffectOpVal), Bool(e.Public)),
            Binding.Trait t => D(PreludeAbi.Tags.Decl.DeclTrait, ReflectId(t.Name), List([t.Param], ReflectId), Fields(t.Fields), Bool(t.Public)),
            // A signature's impl has no fields; reflected, it is an impl with none.
            Binding.Impl i => D(PreludeAbi.Tags.Decl.DeclImpl, Option(i.Name, ReflectId), Path(i.TraitPath), List([i.Arg], ReflectExpr), Fields(i.Fields ?? []), Bool(i.Public)),
            Binding.Macro m => D(PreludeAbi.Tags.Decl.DeclMacro, ReflectId(m.Name), ReflectExpr(m.Value), Bool(m.Public), OptionOf(m.Kind is { } k ? Ann(k) : null), OptionOf(m.Output is null ? null : ReflectExpr(m.Output))),
            Binding.MacroCall c => D(PreludeAbi.Tags.Decl.DeclMacroCall, ReflectExpr(c.Head), List(c.Args, ReflectCaptured), Bool(c.Public)),
            Binding.Field f => D(PreludeAbi.Tags.Decl.DeclField, Str(f.Name), ReflectExpr(f.Type)),
            Binding.Open o => D(PreludeAbi.Tags.Decl.DeclOpen, ReflectExpr(o.Of), Str(o.Label)),
            Binding.Export e => D(PreludeAbi.Tags.Decl.DeclExport, ReflectExpr(e.Of), Option(e.Names is { } names ? (object)names : null, n => List((EquatableArray<string>)n, Str)), Bool(e.Public)),
            Binding.Hole h => D(PreludeAbi.Tags.Decl.DeclHole, ReflectId(h.Name)),
            Binding.SyntaxDecl s => D(PreludeAbi.Tags.Decl.DeclSyntax, ReflectId(s.Name), RoleVal(s.Role), Bool(s.Public)),
            Binding.Items i => D(PreludeAbi.Tags.Decl.DeclItems, ReflectTokens(i.Terms)),
            Binding.Instantiate i => D(PreludeAbi.Tags.Decl.DeclInstantiate, ReflectId(i.Instantiation.Form), RuleVal(i.Instantiation.Rule),
                Captures(i.Instantiation.Captures), Option(i.Instantiation.FromUnit, Str), Bool(i.Public)),
            _ => throw new InvalidOperationException($"unhandled binding {b.GetType().Name}"),
        };
    }

    public Value ReflectDecls(IEnumerable<Binding> bs) => List(bs, ReflectDecl);

    /// <summary>
    /// A macro argument as the value of its parameter's kind (M9): a block is the
    /// <c>Expr</c> it stands as, an id the <c>Id</c> its token names.
    /// </summary>
    public Value ReflectCapture(Capture c) => c switch
    {
        Capture.Expr e => ReflectExpr(e.Syntax),
        Capture.Block b => ReflectExpr(new Syntax.Block(b.Terms, BlockSpan(b.Terms))),
        Capture.Id i => ReflectId(SyntaxMapper.TokenId(i.Token)),
        Capture.Pattern p => ReflectPattern(p.Value),
        Capture.Decls d => ReflectDecls(d.Bindings),
        Capture.Decl d => ReflectDecl(d.Binding),
        Capture.Tokens t => ReflectTokens(t.Terms),
        _ => throw new InvalidOperationException($"unhandled capture {c.GetType().Name}"),
    };

    private static SourceSpan BlockSpan(EquatableArray<Fun.Kernel.TokenTree> terms) =>
        terms.IsEmpty ? SourceSpan.Synthetic : SourceSpan.Between(terms[0].Span, terms[^1].Span);

    /// <summary>A type binder's solution as the macro receives it: <c>RExpr(T)</c>.</summary>
    public Value ReflectType(Value type) => Con(RType, PreludeAbi.Tags.R.RExpr, type);

    // ---- reading values back -------------------------------------------------

    /// <summary>The tag and payload of a constructor of <paramref name="nominal"/>; null for any other value.</summary>
    private (string Name, EquatableArray<Value> Args)? Payload(Value.VNominal nominal, Value v) =>
        Nbe.Force(_metas, v) is Value.VCon c && ReferenceEquals(c.Nominal.Decl, nominal.Decl) ? (c.Name, c.Args) : null;

    private static string? ReadStr(Value v) => v is Value.VAtom { Atom: Atom.Str s } ? s.Value : null;
    private static long? ReadI64(Value v) => v is Value.VAtom { Atom: Atom.I64 n } ? n.Value : null;

    private Value? RecordField(Value v, string name) =>
        Nbe.Force(_metas, v) is Value.VRecord r ? r.Fields.FirstOrDefault(f => f.Name == name).Value : null;

    /// <summary>A value of <paramref name="nominal"/>: the tag of its constructor, "" for any other value.</summary>
    private string Tag(Value.VNominal nominal, Value v) => Payload(nominal, v) is { } payload ? payload.Name : "";

    private bool? ReadBool(Value v) => _bool.CodeOf(Tag(_bool.Nominal, v)) switch
    {
        0 => false,
        1 => true,
        _ => null,
    };

    /// <summary>
    /// An option: <c>(true, null)</c> for None, <c>(true, x)</c> for Some read by
    /// <paramref name="f"/>, <c>(false, _)</c> when malformed.
    /// </summary>
    private (bool Ok, T? Value) ReadOption<T>(Value v, Func<Value, T?> f) where T : class =>
        Payload(_some.Nominal, v) is (var name, var args)
            ? name == _none.Tag ? (true, null)
            : name == _some.Tag && args is [var x] && f(x) is { } read ? (true, read)
            : (false, null)
            : (false, null);

    private (bool Ok, T? Value) ReadOptionS<T>(Value v, Func<Value, T?> f) where T : struct =>
        Payload(_some.Nominal, v) is (var name, var args)
            ? name == _none.Tag ? (true, null)
            : name == _some.Tag && args is [var x] && f(x) is { } read ? (true, read)
            : (false, null)
            : (false, null);

    private EquatableArray<T>? ReadList<T>(Value v, Func<Value, T?> f) where T : class
    {
        var items = new List<T>();
        while (true)
        {
            switch (Payload(_cons.Nominal, v))
            {
                case (var name, _) when name == _nil.Tag: return [.. items];
                case (var name, [var head, var tail]) when name == _cons.Tag && f(head) is { } item:
                    items.Add(item);
                    v = tail;
                    continue;
                default: return null;
            }
        }
    }

    private EquatableArray<T>? ReadListS<T>(Value v, Func<Value, T?> f) where T : struct
    {
        var items = new List<T>();
        while (true)
        {
            switch (Payload(_cons.Nominal, v))
            {
                case (var name, _) when name == _nil.Tag: return [.. items];
                case (var name, [var head, var tail]) when name == _cons.Tag && f(head) is { } item:
                    items.Add(item);
                    v = tail;
                    continue;
                default: return null;
            }
        }
    }

    private sealed record Boxed<T>(T Value);

    private SourceSpan? ReadSpan(Value v)
    {
        var (ok, record) = ReadOption(v, x => Nbe.Force(_metas, x) as Value.VRecord);
        if (!ok) return null;
        if (record is null) return SourceSpan.Synthetic;
        // The probe's field order is the struct's own: file, start_byte, end_byte,
        // start_line, start_col, end_line, end_col.
        var (fileField, startByte, endByte, startLine, startCol, endLine, endCol) =
            (_span.Fields[0], _span.Fields[1], _span.Fields[2], _span.Fields[3], _span.Fields[4], _span.Fields[5], _span.Fields[6]);
        int? IntField(string name) => RecordField(record, name) is { } f && ReadI64(f) is long n ? (int)n : null;
        (bool, int?) OptInt(string name) => RecordField(record, name) is { } f ? ReadOptionS(f, x => ReadI64(x) is long n ? (int?)(int)n : null) : (false, null);
        if (RecordField(record, fileField) is not { } fileV) return null;
        var (fileOk, file) = ReadOption(fileV, ReadStr);
        if (!fileOk || IntField(startByte) is not int start || IntField(endByte) is not int end) return null;
        var (slOk, sl) = OptInt(startLine);
        var (scOk, sc) = OptInt(startCol);
        var (elOk, el) = OptInt(endLine);
        var (ecOk, ec) = OptInt(endCol);
        if (!(slOk && scOk && elOk && ecOk)) return null;
        return SourceSpan.Make(start, end, file, sl, sc, el, ec);
    }

    public Id? ReadId(Value v)
    {
        // The probe's field order is the struct's own: name, span, scope.
        var (nameField, spanField, scopeField) = (_id.Fields[0], _id.Fields[1], _id.Fields[2]);
        if (RecordField(v, nameField) is not { } n || ReadStr(n) is not { } name) return null;
        if (RecordField(v, spanField) is not { } s || ReadSpan(s) is not { } span) return null;
        if (RecordField(v, scopeField) is not Value.VAtom { Atom: Atom.Scopes scopes }) return null;
        return Certified(name, scopes.ResolvedName) ? new Id(name, span, scopes.Set) : null;
    }

    /// <summary>A leaf enum's code as its C# enum: the probe's tags are matched, never spelled.</summary>
    private T? ReadLeaf<T>(Leafs leafs, Value v) where T : struct, Enum
    {
        var code = leafs.CodeOf(Tag(leafs.Nominal, v));
        return code < 0 ? null : (T)Enum.ToObject(typeof(T), code);
    }

    private Explicitness? ReadExplicitness(Value v) => ReadLeaf<Explicitness>(_explicitness, v);

    private Atom? ReadAtom(Value v) => Payload(_atomVal, v) switch
    {
        (PreludeAbi.Tags.AtomVal.I64Atom, [Value.VAtom { Atom: Atom.I64 n }]) => n,
        (PreludeAbi.Tags.AtomVal.CharAtom, [Value.VAtom { Atom: Atom.Char c }]) => c,
        (PreludeAbi.Tags.AtomVal.StringAtom, [Value.VAtom { Atom: Atom.Str s }]) => s,
        (PreludeAbi.Tags.AtomVal.UnitAtom, { IsEmpty: true }) => Atom.Unit.Instance,
        (PreludeAbi.Tags.AtomVal.ScopesAtom, [Value.VAtom { Atom: Atom.Scopes s }]) => s,
        _ => null,
    };

    private AtomTy? ReadAtomTy(Value v) => ReadLeaf<AtomTy>(_atomTy, v);

    /// <summary>A <c>Syntax.Path</c> as the form it names: its head, an open choice when it has one, then each member.</summary>
    private Syntax? ReadPath(Value v)
    {
        // The probe's field order is the struct's own: head, members, head_choice.
        var (headField, membersField, choiceField) = (_path.Fields[0], _path.Fields[1], _path.Fields[2]);
        if (RecordField(v, headField) is not { } h || ReadId(h) is not { } head) return null;
        if (RecordField(v, membersField) is not { } m || ReadList(m, x => ReadStr(x)) is not { } members) return null;
        if (RecordField(v, choiceField) is not { } c) return null;
        // The choice probe's field order is the struct's own: opens, fallback.
        var (opensField, fallbackField) = (_pathChoice.Fields[0], _pathChoice.Fields[1]);
        var (choiceOk, choice) = ReadOption(c, x =>
            RecordField(x, opensField) is { } o && ReadList(o, ReadStr) is { } opens
            && RecordField(x, fallbackField) is { } f && ReadOption(f, ReadStr) is (true, var fallback)
                ? new Boxed<(EquatableArray<string>, string?)>((opens, fallback))
                : null);
        if (!choiceOk) return null;
        Syntax form = choice is null ? new Syntax.Var(head) : new Syntax.OpenChoice(head, choice.Value.Item1, choice.Value.Item2);
        return members.Aggregate(form, (acc, member) => new Syntax.FieldAccess(acc, member, head.Span));
    }

    private FormKind? ReadAnn(Value v) => ReadLeaf<FormKind>(_macroAnn, v);

    private Fixity? ReadFixity(Value v) => ReadLeaf<Fixity>(_fixity, v);

    private Delimiter? ReadDelim(Value v) => ReadLeaf<Delimiter>(_delim, v);

    private static readonly TokenKind.Word[] Punctuation =
    [
        TokenKind.LParen, TokenKind.RParen, TokenKind.LBracket, TokenKind.RBracket, TokenKind.LBrace, TokenKind.RBrace,
        TokenKind.Comma, TokenKind.Dot, TokenKind.Colon, TokenKind.Eq, TokenKind.Semi, TokenKind.Bar, TokenKind.ThinArrow,
        TokenKind.DatumComment, TokenKind.Eof,
    ];

    private TokenKind? ReadTokenKind(Value v) => Payload(_tokenKind, v) switch
    {
        (PreludeAbi.Tags.TokenKind.IdentTok, [var s]) when ReadStr(s) is { } name => new TokenKind.Ident(name),
        (PreludeAbi.Tags.TokenKind.OperatorTok, [var s]) when ReadStr(s) is { } op => new TokenKind.Operator(op),
        (PreludeAbi.Tags.TokenKind.IntTok, [Value.VAtom { Atom: Atom.I64 n }]) => new TokenKind.Int(n.Value),
        (PreludeAbi.Tags.TokenKind.CharTok, [Value.VAtom { Atom: Atom.Char c }]) => new TokenKind.Char(c.Value),
        (PreludeAbi.Tags.TokenKind.StringTok, [var s]) when ReadStr(s) is { } str => new TokenKind.Str(str),
        (PreludeAbi.Tags.TokenKind.KeywordTok, [var s]) when ReadStr(s) is { } kw && TokenKind.Keywords.TryGetValue(kw, out var word) => word,
        (PreludeAbi.Tags.TokenKind.PunctTok, [var s]) when ReadStr(s) is { } p => Punctuation.FirstOrDefault(w => w.Spelling == p),
        _ => null,
    };

    public Fun.Kernel.TokenTree? ReadTokenTree(Value v) => Payload(TokenTreeType, v) switch
    {
        // A reflected `UnitTok` is `()`: the port's tree has no unit token kind, but
        // the same source reads as the empty paren group it is here, and the
        // enforester reads that as unit (the prototype's Token_tree.Unit).
        (PreludeAbi.Tags.TokenTree.Tok, [var unitSpan, var unitKind, _])
            when ReadSpan(unitSpan) is { } unitSp && Payload(_tokenKind, unitKind) is (PreludeAbi.Tags.TokenKind.UnitTok, { IsEmpty: true })
            => new Fun.Kernel.TokenTree.Group(Delimiter.Paren, [], unitSp),
        (PreludeAbi.Tags.TokenTree.Tok, [var span, var kind, Value.VAtom { Atom: Atom.Scopes scopes }])
            when ReadSpan(span) is { } sp && ReadTokenKind(kind) is { } k
                 && (k is not TokenKind.Ident i || Certified(i.Name, scopes.ResolvedName))
            => new Fun.Kernel.TokenTree.Leaf(new Token(k, sp, scopes.Set)),
        (PreludeAbi.Tags.TokenTree.TokGroup, [var span, var d, var items])
            when ReadSpan(span) is { } sp && ReadDelim(d) is { } delim && ReadList(items, ReadTokenTree) is { } ts
            => new Fun.Kernel.TokenTree.Group(delim, ts, sp),
        _ => null,
    };

    public EquatableArray<Fun.Kernel.TokenTree>? ReadTokens(Value v) => ReadList(v, ReadTokenTree);

    private Assoc? ReadAssoc(Value v) => ReadLeaf<Assoc>(_assoc, v);

    private HoleKind? ReadHoleKind(Value v) => ReadLeaf<HoleKind>(_holeKind, v);

    public Syntax? ReadExpr(Value v)
    {
        if (Payload(ExprType, v) is not (var name, [var spanV, .. var args])) return null;
        if (ReadSpan(spanV) is not { } span) return null;
        Syntax? X(Value x) => ReadExpr(x);

        switch (name, args.Length)
        {
            case (PreludeAbi.Tags.Expr.RawVar, 1): return ReadId(args[0]) is { } id ? new Syntax.Var(id) : null;
            case (PreludeAbi.Tags.Expr.RawAtom, 1): return ReadAtom(args[0]) is { } a ? new Syntax.Atom(a, span) : null;
            case (PreludeAbi.Tags.Expr.RawSelf, 0): return new Syntax.Self(span);
            case (PreludeAbi.Tags.Expr.RawSelfType, 0): return new Syntax.SelfType(span);
            case (PreludeAbi.Tags.Expr.RawAp, 3):
                return X(args[0]) is { } f && ReadExplicitness(args[1]) is { } apEx && X(args[2]) is { } arg
                    ? new Syntax.Ap(f, apEx, arg, span) : null;
            case (PreludeAbi.Tags.Expr.RawLam, 2):
                return ReadParam(args[0]) is { } p && X(args[1]) is { } lamBody ? new Syntax.Lam(p, lamBody, span) : null;
            case (PreludeAbi.Tags.Expr.RawLet, 5):
            {
                if (ReadId(args[0]) is not { } n || ReadOption(args[1], X) is not (true, var type)) return null;
                return X(args[2]) is { } value && X(args[3]) is { } body && ReadBool(args[4]) is { } rec
                    ? new Syntax.Let(n, type, value, body, rec, span) : null;
            }
            case (PreludeAbi.Tags.Expr.RawLetRecGroup, 3):
            {
                if (ReadList(args[0], ReadId) is not { } names || ReadList(args[1], X) is not { } values || X(args[2]) is not { } body) return null;
                return names.Length == values.Length
                    ? new Syntax.LetRecGroup([.. names.Zip(values, (n, x) => new RecMember(n, x))], body, span) : null;
            }
            case (PreludeAbi.Tags.Expr.RawAnnotated, 2):
                return X(args[0]) is { } inner && X(args[1]) is { } typ ? new Syntax.Annotated(inner, typ, span) : null;
            case (PreludeAbi.Tags.Expr.RawProd, 1): return ReadList(args[0], X) is { } items ? new Syntax.Prod(items, span) : null;
            case (PreludeAbi.Tags.Expr.RawProdTy, 1): return ReadList(args[0], X) is { } tys ? new Syntax.ProdTy(tys, span) : null;
            case (PreludeAbi.Tags.Expr.RawTraitBoundSet, 1): return ReadList(args[0], X) is { } traits ? new Syntax.TraitBoundSet(traits, span) : null;
            case (PreludeAbi.Tags.Expr.RawArrow, 5):
            {
                if (ReadExplicitness(args[0]) is not { } ex || ReadOption(args[1], ReadId) is not (true, var aname)) return null;
                if (X(args[2]) is not { } dom || ReadOption(args[3], ReadEffectRow) is not (true, var row) || X(args[4]) is not { } cod) return null;
                return new Syntax.Arrow(ex, aname, dom, row, cod, span);
            }
            case (PreludeAbi.Tags.Expr.RawFieldAccess, 2):
                return X(args[0]) is { } of && ReadStr(args[1]) is { } field ? new Syntax.FieldAccess(of, field, span) : null;
            case (PreludeAbi.Tags.Expr.RawProj, 2):
                return X(args[0]) is { } pOf && ReadI64(args[1]) is long idx ? new Syntax.Proj(pOf, (int)idx, span) : null;
            case (PreludeAbi.Tags.Expr.RawRecordConstruct, 2):
                return X(args[0]) is { } rtyp && ReadFields(args[1]) is { } fields ? new Syntax.RecordConstruct(rtyp, fields, span) : null;
            case (PreludeAbi.Tags.Expr.RawStruct, 1): return ReadDecls(args[0]) is { } sb ? new Syntax.Struct(sb, span) : null;
            case (PreludeAbi.Tags.Expr.RawModule, 1): return ReadDecls(args[0]) is { } mb ? new Syntax.Module(mb, span) : null;
            case (PreludeAbi.Tags.Expr.RawSig, 1): return ReadDecls(args[0]) is { } gb ? new Syntax.Sig(gb, span) : null;
            case (PreludeAbi.Tags.Expr.RawEnum, 2):
            {
                if (ReadOption(args[0], ReadStr) is not (true, _)) return null;
                var ctors = ReadList(args[1], c => Payload(_ctor, c) is (PreludeAbi.Tags.Ctor.MkCtor, [var cid, var payloads])
                    && ReadId(cid) is { } ci && ReadList(payloads, X) is { } ps ? new EnumConstructor(ci.Name, ps) : null);
                return ctors is { } cs ? new Syntax.Enum(cs, span) : null;
            }
            case (PreludeAbi.Tags.Expr.RawImport, 2):
                return ReadStr(args[0]) is { } path && args[1] is Value.VAtom { Atom: Atom.Scopes sc }
                    ? new Syntax.Import(path, span) { Scope = sc.Set } : null;
            case (PreludeAbi.Tags.Expr.RawOpen, 3):
                return X(args[0]) is { } m && X(args[1]) is { } obody && ReadStr(args[2]) is { } label
                    ? new Syntax.Open(m, obody, label, span) : null;
            case (PreludeAbi.Tags.Expr.RawOpenChoice, 3):
            {
                if (ReadId(args[0]) is not { } cname || ReadList(args[1], ReadStr) is not { } opens) return null;
                return ReadOption(args[2], ReadStr) is (true, var fallback) ? new Syntax.OpenChoice(cname, opens, fallback) : null;
            }
            case (PreludeAbi.Tags.Expr.RawEffectDef, 4):
                return ReadId(args[0]) is { } ename && ReadList(args[1], ReadId) is { } eps
                       && ReadList(args[2], ReadEffectOp) is { } ops && X(args[3]) is { } ebody
                    ? new Syntax.EffectDef(ename, eps, ops, ebody, span) : null;
            case (PreludeAbi.Tags.Expr.RawTraitDef, 4):
            {
                if (ReadId(args[0]) is not { } tname || ReadList(args[1], ReadId) is not { } tps
                    || ReadFields(args[2]) is not { } tfields || X(args[3]) is not { } tbody) return null;
                return tps.Length == 1
                    ? new Syntax.TraitDef(tname, tps[0], tfields, tbody, span)
                    : throw new FunException("trait declaration accepts exactly one parameter");
            }
            case (PreludeAbi.Tags.Expr.RawImplDef, 5):
            {
                if (ReadOption(args[0], ReadId) is not (true, var iname) || ReadPath(args[1]) is not { } trait
                    || ReadList(args[2], X) is not { } iargs || ReadFields(args[3]) is not { } ifields || X(args[4]) is not { } ibody) return null;
                return iargs.Length == 1
                    ? new Syntax.ImplDef(iname, trait, iargs[0], ifields, ibody, span)
                    : throw new FunException("impl declaration accepts exactly one trait argument");
            }
            case (PreludeAbi.Tags.Expr.RawPerform, 2):
                return ReadPath(args[0]) is Syntax.FieldAccess op && X(args[1]) is { } parg ? new Syntax.Perform(op, parg, span) : null;
            case (PreludeAbi.Tags.Expr.RawResume, 1): return X(args[0]) is { } ra ? new Syntax.Resume(ra, span) : null;
            case (PreludeAbi.Tags.Expr.RawRefNew, 1): return X(args[0]) is { } na ? new Syntax.RefNew(na, span) : null;
            case (PreludeAbi.Tags.Expr.RawRefGet, 1): return X(args[0]) is { } ga ? new Syntax.RefGet(ga, span) : null;
            case (PreludeAbi.Tags.Expr.RawRefSet, 2): return X(args[0]) is { } sr && X(args[1]) is { } sv ? new Syntax.RefSet(sr, sv, span) : null;
            case (PreludeAbi.Tags.Expr.RawMatch, 2):
                return X(args[0]) is { } scrut && ReadList(args[1], ReadBranch) is { } branches ? new Syntax.Match(scrut, branches, span) : null;
            case (PreludeAbi.Tags.Expr.RawStx, 1): return X(args[0]) is { } inner2 ? new Syntax.Stx(inner2, span) : null;
            case (PreludeAbi.Tags.Expr.RawQuote, 2):
                return X(args[0]) is { } template && ReadQuoteHoles(args[1]) is { } holes ? new Syntax.Quote(template, holes, span) : null;
            case (PreludeAbi.Tags.Expr.RawQuoteDecls, 2):
                return ReadDecls(args[0]) is { } qitems && ReadQuoteHoles(args[1]) is { } qholes ? new Syntax.QuoteDecls(qitems, qholes, span) : null;
            case (PreludeAbi.Tags.Expr.RawMacroDef, 5):
            {
                if (ReadId(args[0]) is not { } mname || X(args[1]) is not { } mvalue || X(args[2]) is not { } mbody) return null;
                if (ReadOptionS(args[3], ReadAnn) is not (true, var kind) || ReadOption(args[4], X) is not (true, var output)) return null;
                return new Syntax.MacroDef(mname, mvalue, mbody, kind, output, span);
            }
            case (PreludeAbi.Tags.Expr.RawSyntaxDef, 3):
                return ReadId(args[0]) is { } sname && ReadRole(args[1]) is { } role && X(args[2]) is { } sbody
                    ? new Syntax.SyntaxDef(sname, role, sbody, span) : null;
            case (PreludeAbi.Tags.Expr.RawBlock, 1): return ReadTokens(args[0]) is { } ts ? new Syntax.Block(ts, span) : null;
            case (PreludeAbi.Tags.Expr.RawInstantiate, 4):
                return ReadInstantiation(args[0], args[1], args[2], args[3]) is { } inst ? new Syntax.Instantiate(inst, span) : null;
            case (PreludeAbi.Tags.Expr.RawMacroCall, 2):
                return X(args[0]) is { } head && ReadList(args[1], ReadCaptured) is { } cargs ? new Syntax.MacroCall(head, cargs, span) : null;
            case (PreludeAbi.Tags.Expr.RawOperatorUse, 6):
            {
                if (ReadId(args[0]) is not { } operatorId || ReadFixity(args[1]) is not { } fx || ReadList(args[2], X) is not { } operands) return null;
                if (ReadSpan(args[3]) is not { } declared || ReadSpan(args[4]) is not { } used || ReadOption(args[5], ReadStr) is not (true, var unit)) return null;
                return new Syntax.OperatorUse(operatorId, fx, operands, declared, unit, used);
            }
            case (PreludeAbi.Tags.Expr.RawTypeDef, _):
                throw new FunException("type is a macro: the reflected syntax has no type definition");
            default:
                return null;
        }
    }

    private Instantiation? ReadInstantiation(Value form, Value rule, Value captures, Value fromUnit)
    {
        if (ReadId(form) is not { } f || ReadRule(rule) is not { } r || ReadCaptures(captures) is not { } cs) return null;
        return ReadOption(fromUnit, ReadStr) is (true, var unit) ? new Instantiation(f, r, cs, unit) : null;
    }

    private Role? ReadRole(Value v)
    {
        if (Payload(_role, v) is not (PreludeAbi.Tags.Role.MkRole, [var fixity, var order, var meaning, var declared, var fromUnit])) return null;
        if (ReadFixity(fixity) is not { } fx || ReadOption(order, ReadOrder) is not (true, var ord)) return null;
        RoleMeaning? m = Payload(_roleMeaning, meaning) switch
        {
            (PreludeAbi.Tags.RoleMeaning.ApplyValue, { IsEmpty: true }) => RoleMeaning.ApplyValue.Instance,
            (PreludeAbi.Tags.RoleMeaning.AssignRef, { IsEmpty: true }) => RoleMeaning.AssignRef.Instance,
            (PreludeAbi.Tags.RoleMeaning.CallMacro, { IsEmpty: true }) => RoleMeaning.CallMacro.Instance,
            (PreludeAbi.Tags.RoleMeaning.Rules, [var kind, var rules]) when ReadAnn(kind) is { } k && ReadList(rules, ReadRule) is { } rs => new RoleMeaning.Rules(k, rs),
            (PreludeAbi.Tags.RoleMeaning.OrderGroup, { IsEmpty: true }) => RoleMeaning.OrderGroup.Instance,
            (PreludeAbi.Tags.RoleMeaning.PolyArrow, { IsEmpty: true }) => RoleMeaning.PolyArrow.Instance,
            _ => null,
        };
        if (m is null || ReadSpan(declared) is not { } at) return null;
        return ReadOption(fromUnit, ReadStr) is (true, var unit) ? new Role(fx, ord, m, at, unit) : null;
    }

    private Order? ReadOrder(Value v) => Payload(_order, v) is (PreludeAbi.Tags.Order.MkOrder, [var g, var n, var a, var w, var st, var wk])
        && ReadStr(g) is { } group && ReadStr(n) is { } name && ReadAssoc(a) is { } assoc && ReadBool(w) is { } weakest
        && ReadList(st, ReadOrder) is { } stronger && ReadList(wk, ReadOrder) is { } weaker
            ? new Order(group, name, assoc, weakest, stronger, weaker)
            : null;

    private Rule? ReadRule(Value v)
    {
        if (Payload(_rule, v) is not (PreludeAbi.Tags.Rule.MkRule, [var pattern, var replacement, var span])) return null;
        if (ReadList(pattern, ReadRulePart) is not { } parts || ReadSpan(span) is not { } sp) return null;
        Replacement? r = Payload(_replacement, replacement) switch
        {
            (PreludeAbi.Tags.Replacement.ReplaceExpr, [var e]) when ReadExpr(e) is { } x => new Replacement.Expr(x),
            (PreludeAbi.Tags.Replacement.ReplaceDecls, [var ds]) when ReadDecls(ds) is { } bs => new Replacement.Decls(bs),
            _ => null,
        };
        return r is null ? null : new Rule(parts, r, sp);
    }

    private RulePart? ReadRulePart(Value v) => Payload(_rulePart, v) switch
    {
        (PreludeAbi.Tags.RulePart.PartToken, [var t]) when ReadTokenTree(t) is { } tree => new RulePart.Literal(tree),
        (PreludeAbi.Tags.RulePart.PartGroup, [var d, var parts, var span]) when ReadDelim(d) is { } delim && ReadList(parts, ReadRulePart) is { } ps && ReadSpan(span) is { } sp
            => new RulePart.Group(delim, ps, sp),
        (PreludeAbi.Tags.RulePart.PartHole, [var hole, var kind, var span]) when ReadStr(hole) is { } h && ReadHoleKind(kind) is { } k && ReadSpan(span) is { } sp
            => new RulePart.Hole(h, k, sp),
        _ => null,
    };

    private Capture? ReadCaptured(Value v) => Payload(_captured, v) switch
    {
        (PreludeAbi.Tags.Captured.CapExpr, [var e]) when ReadExpr(e) is { } x => new Capture.Expr(x),
        (PreludeAbi.Tags.Captured.CapBlock, [var ts]) when ReadTokens(ts) is { } terms => new Capture.Block(terms),
        (PreludeAbi.Tags.Captured.CapId, [var t]) when ReadTokenTree(t) is Fun.Kernel.TokenTree.Leaf leaf => new Capture.Id(leaf.Token),
        (PreludeAbi.Tags.Captured.CapPattern, [var p]) when ReadPattern(p) is { } pat => new Capture.Pattern(pat),
        (PreludeAbi.Tags.Captured.CapDecls, [var ds]) when ReadDecls(ds) is { } bs => new Capture.Decls(bs),
        (PreludeAbi.Tags.Captured.CapDecl, [var d]) when ReadDecl(d) is { } b => new Capture.Decl(b),
        (PreludeAbi.Tags.Captured.CapTokens, [var ts]) when ReadTokens(ts) is { } terms => new Capture.Tokens(terms),
        _ => null,
    };

    private EquatableArray<(string Hole, Capture Capture)>? ReadCaptures(Value v) =>
        ReadListS(v, c => Payload(_capture, c) is (PreludeAbi.Tags.Capture.MkCapture, [var n, var captured]) && ReadStr(n) is { } name && ReadCaptured(captured) is { } cap
            ? (name, cap) : ((string, Capture)?)null);

    private EquatableArray<(string Hole, Syntax Value)>? ReadQuoteHoles(Value v) =>
        ReadListS(v, h => Payload(_quoteHole, h) is (PreludeAbi.Tags.QuoteHole.MkQuoteHole, [var n, var e]) && ReadStr(n) is { } name && ReadExpr(e) is { } x
            ? (name, x) : ((string, Syntax)?)null);

    private EquatableArray<(string Name, Syntax Value)>? ReadFields(Value v) =>
        ReadListS(v, f => Payload(_field, f) is (PreludeAbi.Tags.Field.MkField, [var n, var e]) && ReadStr(n) is { } name && ReadExpr(e) is { } x
            ? (name, x) : ((string, Syntax)?)null);

    private Param? ReadParam(Value v)
    {
        if (Payload(_param, v) is not (PreludeAbi.Tags.Param.MkParam, [var n, var ty, var bounds, var ex])) return null;
        if (ReadId(n) is not { } name || ReadOption(ty, ReadExpr) is not (true, var type) || ReadExplicitness(ex) is not { } e) return null;
        if (ReadList(bounds, ReadPath) is not { } bs) return null;
        return new Param(name, type, e, bs);
    }

    private EffectRow? ReadEffectRow(Value v) => Payload(_effectRow, v) is (PreludeAbi.Tags.EffectRow.MkEffectRow, [var effects, var tails, var inferred, var poly])
        && ReadList(effects, ReadExpr) is { } es && ReadList(tails, ReadExpr) is { } ts && ReadBool(inferred) is { } inf && ReadBool(poly) is { } p
            ? new EffectRow(es, ts, inf, p)
            : null;

    private EffectOp? ReadEffectOp(Value v) => Payload(_effectOp, v) is (PreludeAbi.Tags.EffectOp.MkEffectOp, [var n, var input, var output])
        && ReadStr(n) is { } name && ReadExpr(input) is { } i && ReadExpr(output) is { } o
            ? new EffectOp(name, i, o)
            : null;

    private MatchBranch? ReadBranch(Value v) => Payload(_branch, v) switch
    {
        (PreludeAbi.Tags.Branch.ValueBranch, [var p, var body]) when ReadPattern(p) is { } pat && ReadExpr(body) is { } b => new MatchBranch(pat, b),
        (PreludeAbi.Tags.Branch.EffectBranch, [var op, var p, var body]) when ReadPath(op) is Syntax.FieldAccess path && ReadPattern(p) is { } pat && ReadExpr(body) is { } b
            => new MatchBranch(pat, b) { Operation = path },
        _ => null,
    };

    public Fun.Kernel.Pattern? ReadPattern(Value v)
    {
        if (Payload(PatternType, v) is not (var name, [var spanV, .. var args]) || ReadOption(spanV, x => Nbe.Force(_metas, x)) is not (true, _)) return null;
        (string, Fun.Kernel.Pattern)? PatField(Value f) => Payload(_patField, f) is (PreludeAbi.Tags.PatField.MkPatField, [var n, var p]) && ReadStr(n) is { } fname
            && ReadOption(p, ReadPattern) is (true, var fp)
                // `{y}` is `{y = y}`: a binder written by the label.
                ? (fname, fp ?? new Fun.Kernel.Pattern.Bind(new Id(fname, SourceSpan.Synthetic)))
                : null;
        return (name, args.Length) switch
        {
            (PreludeAbi.Tags.Pattern.RawPatWild, 0) => Fun.Kernel.Pattern.Wild.Instance,
            (PreludeAbi.Tags.Pattern.RawPatBind, 1) => ReadId(args[0]) is { } id ? new Fun.Kernel.Pattern.Bind(id) : null,
            (PreludeAbi.Tags.Pattern.RawPatCon, 2) => ReadPath(args[0]) is { } head && ReadList(args[1], ReadPattern) is { } ps ? new Fun.Kernel.Pattern.Con(head, ps) : null,
            (PreludeAbi.Tags.Pattern.RawPatAtom, 1) => ReadAtom(args[0]) is { } a ? new Fun.Kernel.Pattern.Atom(a) : null,
            (PreludeAbi.Tags.Pattern.RawPatProd, 1) => ReadList(args[0], ReadPattern) is { } items ? new Fun.Kernel.Pattern.Prod(items) : null,
            (PreludeAbi.Tags.Pattern.RawPatOr, 2) => ReadPattern(args[0]) is { } l && ReadPattern(args[1]) is { } r ? new Fun.Kernel.Pattern.Or(l, r) : null,
            (PreludeAbi.Tags.Pattern.RawPatRecord, 3) => ReadPath(args[0]) is { } typ && ReadListS(args[1], PatField) is { } fs && ReadBool(args[2]) is { } partial
                ? new Fun.Kernel.Pattern.Record(typ, fs, partial) : null,
            (PreludeAbi.Tags.Pattern.RawPatStructType, 2) => ReadListS(args[0], PatField) is { } sfs && ReadBool(args[1]) is { } spartial
                ? new Fun.Kernel.Pattern.StructType(sfs, spartial) : null,
            (PreludeAbi.Tags.Pattern.RawPatType, 1) => ReadAtomTy(args[0]) is { } t ? new Fun.Kernel.Pattern.AtomType(t) : null,
            _ => null,
        };
    }

    public Binding? ReadDecl(Value v)
    {
        if (Payload(DeclType, v) is not (var name, var args)) return null;
        Syntax? X(Value x) => ReadExpr(x);
        switch (name, args.Length)
        {
            case (PreludeAbi.Tags.Decl.DeclLet, 4):
                return ReadId(args[0]) is { } n && X(args[1]) is { } value && ReadBool(args[2]) is { } pub && ReadBool(args[3]) is { } rec
                    ? new Binding.Let(n, value, pub, rec) : null;
            case (PreludeAbi.Tags.Decl.DeclRecGroup, 3):
            {
                if (ReadList(args[0], ReadId) is not { } names || ReadList(args[1], X) is not { } values || ReadBool(args[2]) is not { } gpub) return null;
                return names.Length == values.Length ? new Binding.RecGroup([.. names.Zip(values, (a, b) => new RecMember(a, b))], gpub) : null;
            }
            case (PreludeAbi.Tags.Decl.DeclMethod, 5):
            {
                if (ReadId(args[0]) is not { } mn || ReadList(args[1], ReadParam) is not { } ps) return null;
                if (ReadOption(args[2], ReadEffectRow) is not (true, var row) || X(args[3]) is not { } body || ReadBool(args[4]) is not { } mpub) return null;
                return new Binding.Method(mn, ps, body, mpub, row);
            }
            case (PreludeAbi.Tags.Decl.DeclEffect, 4):
                return ReadId(args[0]) is { } en && ReadList(args[1], ReadId) is { } eps && ReadList(args[2], ReadEffectOp) is { } ops && ReadBool(args[3]) is { } epub
                    ? new Binding.Effect(en, eps, ops, epub) : null;
            case (PreludeAbi.Tags.Decl.DeclTrait, 4):
            {
                if (ReadId(args[0]) is not { } tn || ReadList(args[1], ReadId) is not { } tps || ReadFields(args[2]) is not { } tf || ReadBool(args[3]) is not { } tpub) return null;
                return tps.Length == 1 ? new Binding.Trait(tn, tps[0], tf, tpub) : throw new FunException("trait declaration accepts exactly one parameter");
            }
            case (PreludeAbi.Tags.Decl.DeclImpl, 5):
            {
                if (ReadOption(args[0], ReadId) is not (true, var iname) || ReadPath(args[1]) is not { } trait || ReadList(args[2], X) is not { } iargs
                    || ReadFields(args[3]) is not { } ifields || ReadBool(args[4]) is not { } ipub) return null;
                return iargs.Length == 1 ? new Binding.Impl(iname, trait, iargs[0], ifields, ipub) : throw new FunException("impl declaration accepts exactly one trait argument");
            }
            case (PreludeAbi.Tags.Decl.DeclMacro, 5):
            {
                if (ReadId(args[0]) is not { } man || X(args[1]) is not { } mvalue || ReadBool(args[2]) is not { } mapub) return null;
                if (ReadOptionS(args[3], ReadAnn) is not (true, var kind) || ReadOption(args[4], X) is not (true, var output)) return null;
                return new Binding.Macro(man, mvalue, mapub, kind, output);
            }
            case (PreludeAbi.Tags.Decl.DeclMacroCall, 3):
                return X(args[0]) is { } head && ReadList(args[1], ReadCaptured) is { } cargs && ReadBool(args[2]) is { } cpub
                    ? new Binding.MacroCall(head, cargs, cpub) : null;
            case (PreludeAbi.Tags.Decl.DeclPatternSyn, 4):
                return ReadId(args[0]) is { } sn && ReadList(args[1], ReadId) is { } sps && ReadPattern(args[2]) is { } rhs && ReadBool(args[3]) is { } spub
                    ? new Binding.Let(sn, new Syntax.PatternSynonym(sps, rhs, sn.Span), spub, false) : null;
            case (PreludeAbi.Tags.Decl.DeclField, 2):
                return ReadStr(args[0]) is { } fname && X(args[1]) is { } ftype ? new Binding.Field(fname, ftype) : null;
            case (PreludeAbi.Tags.Decl.DeclOpen, 2):
                return X(args[0]) is { } of && ReadStr(args[1]) is { } label ? new Binding.Open(of, label) : null;
            case (PreludeAbi.Tags.Decl.DeclExport, 3):
            {
                if (X(args[0]) is not { } eof) return null;
                var (namesOk, exported) = ReadOption(args[1], n => ReadList(n, ReadStr) is { } list ? new Boxed<EquatableArray<string>>(list) : null);
                return namesOk && ReadBool(args[2]) is { } xpub ? new Binding.Export(eof, exported?.Value, xpub) : null;
            }
            case (PreludeAbi.Tags.Decl.DeclHole, 1): return ReadId(args[0]) is { } hid ? new Binding.Hole(hid) : null;
            case (PreludeAbi.Tags.Decl.DeclSyntax, 3):
                return ReadId(args[0]) is { } sname && ReadRole(args[1]) is { } role && ReadBool(args[2]) is { } sypub
                    ? new Binding.SyntaxDecl(sname, role, sypub) : null;
            case (PreludeAbi.Tags.Decl.DeclItems, 1): return ReadTokens(args[0]) is { } ts ? new Binding.Items(ts) : null;
            case (PreludeAbi.Tags.Decl.DeclInstantiate, 5):
                return ReadInstantiation(args[0], args[1], args[2], args[3]) is { } inst && ReadBool(args[4]) is { } ipub2
                    ? new Binding.Instantiate(inst, ipub2) : null;
            default:
                return null;
        }
    }

    public EquatableArray<Binding>? ReadDecls(Value v) => ReadList(v, ReadDecl);

    /// <summary>What a declaration macro returns: a list of declarations, or one.</summary>
    public EquatableArray<Binding>? ReadDeclOutput(Value v) =>
        ReadDecls(v) ?? (ReadDecl(v) is { } one ? [one] : null);
}
