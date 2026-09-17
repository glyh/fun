using Fun.Kernel;

namespace Fun.Compiler;

/// <summary>
/// Reflection: <see cref="Syntax"/> and the reflection types of the prelude's
/// <c>Syntax</c> module are one grammar seen twice. <c>Reflect*</c> builds the value
/// of a form and <c>Read*</c> reads a form back; reading what was reflected is the
/// identity on every field (M1). A value that is not well-formed reflection reads
/// back as null, never as a guess. A form the port has no reflected image for, or a
/// reflected form the port cannot read, is "not ported yet".
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
        ExprType = Nominal("Syntax", "Expr");
        DeclType = Nominal("Syntax", "Decl");
        PatternType = Nominal("Syntax", "Pattern");
        TokenTreeType = Nominal("Syntax", "TokenTree");
        RType = Nominal("Syntax", "R");
        _option = Nominal("Option");
        _list = Nominal("List");
        _bool = Nominal("Bool");
        _explicitness = Nominal("Syntax", "Explicitness");
        _atomVal = Nominal("Syntax", "AtomVal");
        _atomTy = Nominal("Syntax", "AtomTy");
        _fixity = Nominal("Syntax", "Fixity");
        _macroAnn = Nominal("Syntax", "MacroAnn");
        _tokenKind = Nominal("Syntax", "TokenKind");
        _delim = Nominal("Syntax", "Delim");
        _assoc = Nominal("Syntax", "Assoc");
        _role = Nominal("Syntax", "Role");
        _order = Nominal("Syntax", "Order");
        _roleMeaning = Nominal("Syntax", "RoleMeaning");
        _rule = Nominal("Syntax", "Rule");
        _rulePart = Nominal("Syntax", "RulePart");
        _holeKind = Nominal("Syntax", "HoleKind");
        _replacement = Nominal("Syntax", "Replacement");
        _capture = Nominal("Syntax", "Capture");
        _captured = Nominal("Syntax", "Captured");
        _field = Nominal("Syntax", "Field");
        _quoteHole = Nominal("Syntax", "QuoteHole");
        _param = Nominal("Syntax", "Param");
        _effectRow = Nominal("Syntax", "EffectRow");
        _effectOp = Nominal("Syntax", "EffectOp");
        _ctor = Nominal("Syntax", "Ctor");
        _branch = Nominal("Syntax", "Branch");
        _patField = Nominal("Syntax", "PatField");
        _idType = Member("Syntax", "Id");
        _spanType = Member("Syntax", "Span");
        _pathType = Member("Syntax", "Path");
        _pathChoiceType = Member("Syntax", "PathChoice");
        DeclsType = Nbe.Force(_metas, Member("Syntax", "Decls"));
        IdType = _idType;
    }

    private static readonly Lazy<Reflection> PreludeReflection =
        new(() => new Reflection(Prelude.Metas, Prelude.Unit.Value));

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

    private readonly Value.VNominal _option, _list, _bool, _explicitness, _atomVal, _atomTy, _fixity, _macroAnn,
        _tokenKind, _delim, _assoc, _role, _order, _roleMeaning, _rule, _rulePart, _holeKind, _replacement,
        _capture, _captured, _field, _quoteHole, _param, _effectRow, _effectOp, _ctor, _branch, _patField;

    private readonly Value _idType, _spanType, _pathType, _pathChoiceType;

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
        while (v is Value.VLam) v = Nbe.Force(_metas, Nbe.Apply(_metas, v, Value.VU.Instance));
        return v as Value.VNominal ?? throw new InvalidOperationException($"the prelude's {string.Join(".", path)} is not a nominal type");
    }

    // ---- building values -----------------------------------------------------

    private static Value Con(Value.VNominal nominal, string name, params Value[] args) => new Value.VCon(name, [.. args], nominal);

    private static Value Str(string s) => new Value.VAtom(new Atom.Str(s));
    private static Value I64(long n) => new Value.VAtom(new Atom.I64(n));

    private Value Bool(bool b) => Con(_bool, b ? "True" : "False");

    private Value Option<T>(T? x, Func<T, Value> f) where T : class =>
        x is null ? Con(_option, "None") : Con(_option, "Some", f(x));

    private Value OptionOf(Value? x) => x is null ? Con(_option, "None") : Con(_option, "Some", x);

    private Value List<T>(IEnumerable<T> items, Func<T, Value> f) =>
        items.Reverse().Aggregate(Con(_list, "Nil"), (tail, head) => Con(_list, "Cons", f(head), tail));

    private static Value Record(Value type, params (string, Value)[] fields) => new Value.VRecord(type, [.. fields]);

    /// <summary>A resolved name carries the certificate that it was minted as one (M12).</summary>
    private static string? Certificate(string name) => name.Contains('#') ? name : null;

    private static bool Certified(string name, string? certificate) => !name.Contains('#') || certificate == name;

    private Value Span(SourceSpan span) => span.IsSynthetic
        ? Con(_option, "None")
        : Con(_option, "Some", Record(_spanType,
            ("file", Option(span.File, Str)),
            ("start_byte", I64(span.Start)),
            ("end_byte", I64(span.End)),
            ("start_line", OptionOf(span.StartLine is int sl ? I64(sl) : null)),
            ("start_col", OptionOf(span.StartCol is int sc ? I64(sc) : null)),
            ("end_line", OptionOf(span.EndLine is int el ? I64(el) : null)),
            ("end_col", OptionOf(span.EndCol is int ec ? I64(ec) : null))));

    public Value ReflectId(Id id) => Record(_idType,
        ("name", Str(id.Name)),
        ("span", Span(id.Span)),
        ("scope", new Value.VAtom(new Atom.Scopes(id.Scope, Certificate(id.Name)))));

    private Value Explicitness(Explicitness e) => Con(_explicitness, e == Kernel.Explicitness.Explicit ? "Explicit" : "Implicit");

    private Value AtomVal(Atom a) => a switch
    {
        Atom.I64 n => Con(_atomVal, "I64Atom", new Value.VAtom(n)),
        Atom.Char c => Con(_atomVal, "CharAtom", new Value.VAtom(c)),
        Atom.Str s => Con(_atomVal, "StringAtom", new Value.VAtom(s)),
        Atom.Unit => Con(_atomVal, "UnitAtom"),
        Atom.Scopes s => Con(_atomVal, "ScopesAtom", new Value.VAtom(s)),
        _ => throw new InvalidOperationException($"unhandled atom {a.GetType().Name}"),
    };

    private static readonly (AtomTy Ty, string Name)[] AtomTyNames =
        [(AtomTy.I64, "TyI64"), (AtomTy.Unit, "TyUnit"), (AtomTy.Char, "TyChar"), (AtomTy.String, "TyString"), (AtomTy.Scopes, "TyScopes"), (AtomTy.Absurd, "TyAbsurd")];

    private Value AtomTyVal(AtomTy t) => Con(_atomTy, AtomTyNames.First(p => p.Ty == t).Name);

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
            Syntax.OpenChoice c => (c.Name, Record(_pathChoiceType, ("opens", List(c.Opens, Str)), ("fallback", Option(c.Fallback, Str)))),
            _ => throw new NotImplementedException($"not ported yet: reflecting a {form.GetType().Name} as a path"),
        };
        return Record(_pathType, ("head", ReflectId(head)), ("members", List(members, Str)), ("head_choice", OptionOf(choice)));
    }

    private Value Ann(FormKind k) => Con(_macroAnn, k == FormKind.Expr ? "AnnExpr" : "AnnDecl");
    private Value FixityVal(Fixity f) => Con(_fixity, f == Fixity.Prefix ? "PrefixFixity" : "InfixFixity");

    private Value Delim(Delimiter d) => Con(_delim, d switch
    {
        Delimiter.Paren => "ParenDelim",
        Delimiter.Bracket => "BracketDelim",
        _ => "BraceDelim",
    });

    private Value TokenKindVal(TokenKind k) => k switch
    {
        TokenKind.Ident i => Con(_tokenKind, "IdentTok", Str(i.Name)),
        TokenKind.Operator o => Con(_tokenKind, "OperatorTok", Str(o.Spelling)),
        TokenKind.Int n => Con(_tokenKind, "IntTok", I64(n.Value)),
        TokenKind.Char c => Con(_tokenKind, "CharTok", new Value.VAtom(new Atom.Char(c.Value))),
        TokenKind.Str s => Con(_tokenKind, "StringTok", Str(s.Value)),
        TokenKind.Word w when TokenKind.Keywords.ContainsKey(w.Spelling) => Con(_tokenKind, "KeywordTok", Str(w.Spelling)),
        TokenKind.Word w => Con(_tokenKind, "PunctTok", Str(w.Spelling)),
        _ => throw new InvalidOperationException($"unhandled token kind {k.GetType().Name}"),
    };

    /// <summary>A token tree, each token with its scope set (M9).</summary>
    public Value ReflectTokenTree(Fun.Kernel.TokenTree t) => t switch
    {
        Fun.Kernel.TokenTree.Leaf { Token: var tok } => Con(TokenTreeType, "Tok", Span(tok.Span), TokenKindVal(tok.Kind),
            new Value.VAtom(new Atom.Scopes(tok.Scope, tok.Kind is TokenKind.Ident i ? Certificate(i.Name) : null))),
        Fun.Kernel.TokenTree.Group g => Con(TokenTreeType, "TokGroup", Span(g.Span), Delim(g.Delimiter), List(g.Items, ReflectTokenTree)),
        _ => throw new InvalidOperationException($"unhandled token tree {t.GetType().Name}"),
    };

    public Value ReflectTokens(IEnumerable<Fun.Kernel.TokenTree> ts) => List(ts, ReflectTokenTree);

    private Value AssocVal(Assoc a) => Con(_assoc, a switch { Assoc.Left => "Left", Assoc.Right => "Right", _ => "NonAssoc" });

    private Value HoleKindVal(HoleKind k) => Con(_holeKind, k switch
    {
        HoleKind.Expr => "HoleExpr",
        HoleKind.Block => "HoleBlock",
        HoleKind.Id => "HoleId",
        HoleKind.Decls => "HoleDecl",
        HoleKind.Decl => "HoleOneDecl",
        HoleKind.Pattern => "HolePattern",
        _ => "HoleTokens",
    });

    public Value ReflectExpr(Syntax stx)
    {
        Value E(string name, params Value[] args) => Con(ExprType, name, [Span(stx.Span), .. args]);
        Value X(Syntax s) => ReflectExpr(s);
        Value? XOpt(Syntax? s) => s is null ? null : ReflectExpr(s);

        return stx switch
        {
            Syntax.Var v => E("RawVar", ReflectId(v.Id)),
            Syntax.Atom a => E("RawAtom", AtomVal(a.Value)),
            Syntax.Self => E("RawSelf"),
            Syntax.SelfType => E("RawSelfType"),
            Syntax.Ap a => E("RawAp", X(a.Fn), Explicitness(a.Explicitness), X(a.Arg)),
            Syntax.Lam l => E("RawLam", ParamVal(l.Param), X(l.Body)),
            Syntax.Let l => E("RawLet", ReflectId(l.Name), OptionOf(XOpt(l.Type)), X(l.Value), X(l.Body), Bool(l.Recursive)),
            Syntax.LetRecGroup g => E("RawLetRecGroup", List(g.Members, m => ReflectId(m.Name)), List(g.Members, m => X(m.Value)), X(g.Body)),
            Syntax.Annotated a => E("RawAnnotated", X(a.Inner), X(a.Type)),
            Syntax.Prod p => E("RawProd", List(p.Items, X)),
            Syntax.ProdTy p => E("RawProdTy", List(p.Items, X)),
            Syntax.TraitBoundSet b => E("RawTraitBoundSet", List(b.Traits, X)),
            Syntax.Arrow a => E("RawArrow", Explicitness(a.Explicitness), Option(a.Name, ReflectId), X(a.Domain), Option(a.Row, EffectRowVal), X(a.Codomain)),
            Syntax.FieldAccess f => E("RawFieldAccess", X(f.Of), Str(f.Field)),
            Syntax.Proj p => E("RawProj", X(p.Of), I64(p.Index)),
            Syntax.RecordConstruct r => E("RawRecordConstruct", X(r.Type), Fields(r.Fields)),
            Syntax.Struct s => E("RawStruct", List(s.Bindings, ReflectDecl)),
            Syntax.Module m => E("RawModule", List(m.Bindings, ReflectDecl)),
            Syntax.Sig s => E("RawSig", List(s.Bindings, ReflectDecl)),
            Syntax.Enum e => E("RawEnum", Con(_option, "None"),
                List(e.Constructors, c => Con(_ctor, "MkCtor", ReflectId(new Id(c.Name, SourceSpan.Synthetic)), List(c.Payloads, X)))),
            Syntax.Import i => E("RawImport", Str(i.Path), new Value.VAtom(new Atom.Scopes(i.Scope, null))),
            Syntax.Open o => E("RawOpen", X(o.Of), X(o.Body), Str(o.Label)),
            Syntax.OpenChoice c => E("RawOpenChoice", ReflectId(c.Name), List(c.Opens, Str), Option(c.Fallback, Str)),
            Syntax.EffectDef d => E("RawEffectDef", ReflectId(d.Name), List(d.Params, ReflectId), List(d.Ops, EffectOpVal), X(d.Body)),
            Syntax.TraitDef t => E("RawTraitDef", ReflectId(t.Name), List([t.Param], ReflectId), Fields(t.Fields), X(t.Body)),
            Syntax.ImplDef i => E("RawImplDef", Option(i.Name, ReflectId), Path(i.TraitPath), List([i.Arg], X), Fields(i.Fields), X(i.Body)),
            Syntax.Perform p => E("RawPerform", Path(p.Operation), X(p.Arg)),
            Syntax.Resume r => E("RawResume", X(r.Arg)),
            Syntax.RefNew n => E("RawRefNew", X(n.Arg)),
            Syntax.RefGet g => E("RawRefGet", X(g.Ref)),
            Syntax.RefSet s => E("RawRefSet", X(s.Ref), X(s.Value)),
            Syntax.Match m => E("RawMatch", X(m.Scrutinee), List(m.Branches, BranchVal)),
            Syntax.Stx s => E("RawStx", X(s.Inner)),
            // A macro sees the argument, not the elaborator's note that it is done.
            Syntax.Elaborated el => X(el.Form),
            Syntax.Quote q => E("RawQuote", X(q.Template), QuoteHoles(q.Holes)),
            Syntax.QuoteDecls q => E("RawQuoteDecls", List(q.Items, ReflectDecl), QuoteHoles(q.Holes)),
            Syntax.MacroDef d => E("RawMacroDef", ReflectId(d.Name), X(d.Value), X(d.Body), OptionOf(d.Kind is { } k ? Ann(k) : null), OptionOf(XOpt(d.Output))),
            Syntax.SyntaxDef d => E("RawSyntaxDef", ReflectId(d.Name), RoleVal(d.Role), X(d.Body)),
            Syntax.Block b => E("RawBlock", ReflectTokens(b.Terms)),
            Syntax.Instantiate i => E("RawInstantiate", ReflectId(i.Instantiation.Form), RuleVal(i.Instantiation.Rule),
                Captures(i.Instantiation.Captures), Option(i.Instantiation.FromUnit, Str)),
            Syntax.MacroCall c => E("RawMacroCall", X(c.Head), List(c.Args, ReflectCaptured)),
            Syntax.OperatorUse u => E("RawOperatorUse", ReflectId(u.Operator), FixityVal(u.Fixity), List(u.Operands, X),
                Span(u.DeclaredAt), Span(u.Span), Option(u.FromUnit, Str)),
            _ => throw new NotImplementedException($"not ported yet: reflecting the form {stx.GetType().Name}"),
        };
    }

    private Value RoleVal(Role r)
    {
        var meaning = r.Meaning switch
        {
            RoleMeaning.ApplyValue => Con(_roleMeaning, "ApplyValue"),
            RoleMeaning.AssignRef => Con(_roleMeaning, "AssignRef"),
            RoleMeaning.CallMacro => Con(_roleMeaning, "CallMacro"),
            RoleMeaning.Rules rules => Con(_roleMeaning, "Rules", Ann(rules.Kind), List(rules.Items, RuleVal)),
            RoleMeaning.OrderGroup => Con(_roleMeaning, "OrderGroup"),
            RoleMeaning.PolyArrow => Con(_roleMeaning, "PolyArrow"),
            _ => throw new InvalidOperationException($"unhandled role meaning {r.Meaning.GetType().Name}"),
        };
        return Con(_role, "MkRole", FixityVal(r.Fixity), Option(r.Order, OrderVal), meaning, Span(r.DeclaredAt), Option(r.FromUnit, Str));
    }

    private Value OrderVal(Order o) => Con(_order, "MkOrder", Str(o.Group), Str(o.Name), AssocVal(o.Assoc), Bool(o.Weakest),
        List(o.StrongerThan, OrderVal), List(o.WeakerThan, OrderVal));

    private Value RuleVal(Rule r) => Con(_rule, "MkRule", List(r.Pattern, RulePartVal),
        r.Replacement switch
        {
            Replacement.Expr e => Con(_replacement, "ReplaceExpr", ReflectExpr(e.Syntax)),
            Replacement.Decls d => Con(_replacement, "ReplaceDecls", List(d.Bindings, ReflectDecl)),
            _ => throw new InvalidOperationException($"unhandled replacement {r.Replacement.GetType().Name}"),
        },
        Span(r.Span));

    private Value RulePartVal(RulePart p) => p switch
    {
        RulePart.Literal l => Con(_rulePart, "PartToken", ReflectTokenTree(l.Term)),
        RulePart.Group g => Con(_rulePart, "PartGroup", Delim(g.Delimiter), List(g.Parts, RulePartVal), Span(g.Span)),
        RulePart.Hole h => Con(_rulePart, "PartHole", Str(h.Name), HoleKindVal(h.Kind), Span(h.Span)),
        _ => throw new InvalidOperationException($"unhandled rule part {p.GetType().Name}"),
    };

    private Value ReflectCaptured(Capture c) => c switch
    {
        Capture.Expr e => Con(_captured, "CapExpr", ReflectExpr(e.Syntax)),
        Capture.Block b => Con(_captured, "CapBlock", ReflectTokens(b.Terms)),
        Capture.Id i => Con(_captured, "CapId", ReflectTokenTree(new Fun.Kernel.TokenTree.Leaf(i.Token))),
        Capture.Pattern p => Con(_captured, "CapPattern", ReflectPattern(p.Value)),
        Capture.Decls d => Con(_captured, "CapDecls", List(d.Bindings, ReflectDecl)),
        Capture.Decl d => Con(_captured, "CapDecl", ReflectDecl(d.Binding)),
        Capture.Tokens t => Con(_captured, "CapTokens", ReflectTokens(t.Terms)),
        _ => throw new InvalidOperationException($"unhandled capture {c.GetType().Name}"),
    };

    private Value Captures(EquatableArray<(string Hole, Capture Capture)> captures) =>
        List(captures, c => Con(_capture, "MkCapture", Str(c.Hole), ReflectCaptured(c.Capture)));

    private Value QuoteHoles(EquatableArray<(string Hole, Syntax Value)> holes) =>
        List(holes, h => Con(_quoteHole, "MkQuoteHole", Str(h.Hole), ReflectExpr(h.Value)));

    private Value Fields(EquatableArray<(string Name, Syntax Value)> fields) =>
        List(fields, f => Con(_field, "MkField", Str(f.Name), ReflectExpr(f.Value)));

    // A binder's trait bounds are written in its type (a TraitBoundSet), so the
    // reflected bound paths are always empty.
    private Value ParamVal(Param p) =>
        Con(_param, "MkParam", ReflectId(p.Name), OptionOf(p.Type is null ? null : ReflectExpr(p.Type)), Con(_list, "Nil"), Explicitness(p.Explicitness));

    private Value EffectRowVal(EffectRow row) =>
        Con(_effectRow, "MkEffectRow", List(row.Effects, ReflectExpr), List(row.Tails, ReflectExpr), Bool(row.Inferred), Bool(row.Polymorphic));

    private Value EffectOpVal(EffectOp op) => Con(_effectOp, "MkEffectOp", Str(op.Name), ReflectExpr(op.Input), ReflectExpr(op.Output));

    private Value BranchVal(MatchBranch b) => b.Operation is { } op
        ? Con(_branch, "EffectBranch", Path(op), ReflectPattern(b.Pattern), ReflectExpr(b.Body))
        : Con(_branch, "ValueBranch", ReflectPattern(b.Pattern), ReflectExpr(b.Body));

    public Value ReflectPattern(Fun.Kernel.Pattern p)
    {
        // Patterns carry no span: the reflected span is always None.
        Value P(string name, params Value[] args) => Con(PatternType, name, [Con(_option, "None"), .. args]);
        Value PatField((string Name, Fun.Kernel.Pattern Pattern) f) => Con(_patField, "MkPatField", Str(f.Name), Con(_option, "Some", ReflectPattern(f.Pattern)));
        return p switch
        {
            Fun.Kernel.Pattern.Wild => P("RawPatWild"),
            Fun.Kernel.Pattern.Bind b => P("RawPatBind", ReflectId(b.Name)),
            Fun.Kernel.Pattern.Con c => P("RawPatCon", Path(c.Head), List(c.Args, ReflectPattern)),
            Fun.Kernel.Pattern.Atom a => P("RawPatAtom", AtomVal(a.Value)),
            Fun.Kernel.Pattern.Prod pr => P("RawPatProd", List(pr.Items, ReflectPattern)),
            Fun.Kernel.Pattern.Or o => P("RawPatOr", ReflectPattern(o.Left), ReflectPattern(o.Right)),
            Fun.Kernel.Pattern.Record r => P("RawPatRecord", Path(r.Type), List(r.Fields, PatField), Bool(r.Partial)),
            Fun.Kernel.Pattern.StructType s => P("RawPatStructType", List(s.Fields, PatField), Bool(s.Partial)),
            Fun.Kernel.Pattern.AtomType t => P("RawPatType", AtomTyVal(t.Ty)),
            _ => throw new NotImplementedException($"not ported yet: reflecting the pattern {p.GetType().Name}"),
        };
    }

    public Value ReflectDecl(Binding b)
    {
        Value D(string name, params Value[] args) => Con(DeclType, name, args);
        return b switch
        {
            Binding.Let { Value: Syntax.PatternSynonym s } l => D("DeclPatternSyn", ReflectId(l.Name), List(s.Params, ReflectId), ReflectPattern(s.Rhs), Bool(l.Public)),
            Binding.Let l => D("DeclLet", ReflectId(l.Name), ReflectExpr(l.Value), Bool(l.Public), Bool(l.Recursive)),
            Binding.RecGroup g => D("DeclRecGroup", List(g.Members, m => ReflectId(m.Name)), List(g.Members, m => ReflectExpr(m.Value)), Bool(g.Public)),
            Binding.Method m => D("DeclMethod", ReflectId(m.Name), List(m.Params, ParamVal), Option(m.Row, EffectRowVal), ReflectExpr(m.Body), Bool(m.Public)),
            Binding.Effect e => D("DeclEffect", ReflectId(e.Name), List(e.Params, ReflectId), List(e.Ops, EffectOpVal), Bool(e.Public)),
            Binding.Trait t => D("DeclTrait", ReflectId(t.Name), List([t.Param], ReflectId), Fields(t.Fields), Bool(t.Public)),
            // A signature's impl has no fields; reflected, it is an impl with none.
            Binding.Impl i => D("DeclImpl", Option(i.Name, ReflectId), Path(i.TraitPath), List([i.Arg], ReflectExpr), Fields(i.Fields ?? []), Bool(i.Public)),
            Binding.Macro m => D("DeclMacro", ReflectId(m.Name), ReflectExpr(m.Value), Bool(m.Public), OptionOf(m.Kind is { } k ? Ann(k) : null), OptionOf(m.Output is null ? null : ReflectExpr(m.Output))),
            Binding.MacroCall c => D("DeclMacroCall", ReflectExpr(c.Head), List(c.Args, ReflectCaptured), Bool(c.Public)),
            Binding.Field f => D("DeclField", Str(f.Name), ReflectExpr(f.Type)),
            Binding.Open o => D("DeclOpen", ReflectExpr(o.Of), Str(o.Label)),
            Binding.Export e => D("DeclExport", ReflectExpr(e.Of), Option(e.Names is { } names ? (object)names : null, n => List((EquatableArray<string>)n, Str)), Bool(e.Public)),
            Binding.Hole h => D("DeclHole", ReflectId(h.Name)),
            Binding.SyntaxDecl s => D("DeclSyntax", ReflectId(s.Name), RoleVal(s.Role), Bool(s.Public)),
            Binding.Items i => D("DeclItems", ReflectTokens(i.Terms)),
            Binding.Instantiate i => D("DeclInstantiate", ReflectId(i.Instantiation.Form), RuleVal(i.Instantiation.Rule),
                Captures(i.Instantiation.Captures), Option(i.Instantiation.FromUnit, Str), Bool(i.Public)),
            _ => throw new NotImplementedException($"not ported yet: reflecting the declaration {b.GetType().Name}"),
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
    public Value ReflectType(Value type) => Con(RType, "RExpr", type);

    // ---- reading values back -------------------------------------------------

    /// <summary>The tag and payload of a constructor of <paramref name="nominal"/>; null for any other value.</summary>
    private (string Name, EquatableArray<Value> Args)? Payload(Value.VNominal nominal, Value v) =>
        Nbe.Force(_metas, v) is Value.VCon c && ReferenceEquals(c.Nominal.Decl, nominal.Decl) ? (c.Name, c.Args) : null;

    private static string? ReadStr(Value v) => v is Value.VAtom { Atom: Atom.Str s } ? s.Value : null;
    private static long? ReadI64(Value v) => v is Value.VAtom { Atom: Atom.I64 n } ? n.Value : null;

    private Value? RecordField(Value v, string name) =>
        Nbe.Force(_metas, v) is Value.VRecord r ? r.Fields.FirstOrDefault(f => f.Name == name).Value : null;

    private bool? ReadBool(Value v) => Payload(_bool, v) switch
    {
        ("True", { IsEmpty: true }) => true,
        ("False", { IsEmpty: true }) => false,
        _ => null,
    };

    /// <summary>
    /// An option: <c>(true, null)</c> for None, <c>(true, x)</c> for Some read by
    /// <paramref name="f"/>, <c>(false, _)</c> when malformed.
    /// </summary>
    private (bool Ok, T? Value) ReadOption<T>(Value v, Func<Value, T?> f) where T : class => Payload(_option, v) switch
    {
        ("None", _) => (true, null),
        ("Some", [var x]) when f(x) is { } read => (true, read),
        _ => (false, null),
    };

    private (bool Ok, T? Value) ReadOptionS<T>(Value v, Func<Value, T?> f) where T : struct => Payload(_option, v) switch
    {
        ("None", _) => (true, null),
        ("Some", [var x]) when f(x) is { } read => (true, read),
        _ => (false, null),
    };

    private EquatableArray<T>? ReadList<T>(Value v, Func<Value, T?> f) where T : class
    {
        var items = new List<T>();
        while (true)
        {
            switch (Payload(_list, v))
            {
                case ("Nil", _): return [.. items];
                case ("Cons", [var head, var tail]) when f(head) is { } item:
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
            switch (Payload(_list, v))
            {
                case ("Nil", _): return [.. items];
                case ("Cons", [var head, var tail]) when f(head) is { } item:
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
        int? IntField(string name) => RecordField(record, name) is { } f && ReadI64(f) is long n ? (int)n : null;
        (bool, int?) OptInt(string name) => RecordField(record, name) is { } f ? ReadOptionS(f, x => ReadI64(x) is long n ? (int?)(int)n : null) : (false, null);
        if (RecordField(record, "file") is not { } fileV) return null;
        var (fileOk, file) = ReadOption(fileV, ReadStr);
        if (!fileOk || IntField("start_byte") is not int start || IntField("end_byte") is not int end) return null;
        var (slOk, sl) = OptInt("start_line");
        var (scOk, sc) = OptInt("start_col");
        var (elOk, el) = OptInt("end_line");
        var (ecOk, ec) = OptInt("end_col");
        if (!(slOk && scOk && elOk && ecOk)) return null;
        return SourceSpan.Make(start, end, file, sl, sc, el, ec);
    }

    public Id? ReadId(Value v)
    {
        if (RecordField(v, "name") is not { } n || ReadStr(n) is not { } name) return null;
        if (RecordField(v, "span") is not { } s || ReadSpan(s) is not { } span) return null;
        if (RecordField(v, "scope") is not Value.VAtom { Atom: Atom.Scopes scopes }) return null;
        return Certified(name, scopes.ResolvedName) ? new Id(name, span, scopes.Set) : null;
    }

    private Explicitness? ReadExplicitness(Value v) => Payload(_explicitness, v) switch
    {
        ("Explicit", { IsEmpty: true }) => Kernel.Explicitness.Explicit,
        ("Implicit", { IsEmpty: true }) => Kernel.Explicitness.Implicit,
        _ => null,
    };

    private Atom? ReadAtom(Value v) => Payload(_atomVal, v) switch
    {
        ("I64Atom", [Value.VAtom { Atom: Atom.I64 n }]) => n,
        ("CharAtom", [Value.VAtom { Atom: Atom.Char c }]) => c,
        ("StringAtom", [Value.VAtom { Atom: Atom.Str s }]) => s,
        ("UnitAtom", { IsEmpty: true }) => Atom.Unit.Instance,
        ("ScopesAtom", [Value.VAtom { Atom: Atom.Scopes s }]) => s,
        _ => null,
    };

    private AtomTy? ReadAtomTy(Value v) =>
        Payload(_atomTy, v) is (var name, { IsEmpty: true }) && AtomTyNames.Any(p => p.Name == name)
            ? AtomTyNames.First(p => p.Name == name).Ty
            : null;

    /// <summary>A <c>Syntax.Path</c> as the form it names: its head, an open choice when it has one, then each member.</summary>
    private Syntax? ReadPath(Value v)
    {
        if (RecordField(v, "head") is not { } h || ReadId(h) is not { } head) return null;
        if (RecordField(v, "members") is not { } m || ReadList(m, x => ReadStr(x)) is not { } members) return null;
        if (RecordField(v, "head_choice") is not { } c) return null;
        var (choiceOk, choice) = ReadOption(c, x =>
            RecordField(x, "opens") is { } o && ReadList(o, ReadStr) is { } opens
            && RecordField(x, "fallback") is { } f && ReadOption(f, ReadStr) is (true, var fallback)
                ? new Boxed<(EquatableArray<string>, string?)>((opens, fallback))
                : null);
        if (!choiceOk) return null;
        Syntax form = choice is null ? new Syntax.Var(head) : new Syntax.OpenChoice(head, choice.Value.Item1, choice.Value.Item2);
        return members.Aggregate(form, (acc, member) => new Syntax.FieldAccess(acc, member, head.Span));
    }

    private FormKind? ReadAnn(Value v) => Payload(_macroAnn, v) switch
    {
        ("AnnExpr", { IsEmpty: true }) => FormKind.Expr,
        ("AnnDecl", { IsEmpty: true }) => FormKind.Decl,
        _ => null,
    };

    private Fixity? ReadFixity(Value v) => Payload(_fixity, v) switch
    {
        ("PrefixFixity", { IsEmpty: true }) => Fixity.Prefix,
        ("InfixFixity", { IsEmpty: true }) => Fixity.Infix,
        _ => null,
    };

    private Delimiter? ReadDelim(Value v) => Payload(_delim, v) switch
    {
        ("ParenDelim", { IsEmpty: true }) => Delimiter.Paren,
        ("BracketDelim", { IsEmpty: true }) => Delimiter.Bracket,
        ("BraceDelim", { IsEmpty: true }) => Delimiter.Brace,
        _ => null,
    };

    private static readonly TokenKind.Word[] Punctuation =
    [
        TokenKind.LParen, TokenKind.RParen, TokenKind.LBracket, TokenKind.RBracket, TokenKind.LBrace, TokenKind.RBrace,
        TokenKind.Comma, TokenKind.Dot, TokenKind.Colon, TokenKind.Eq, TokenKind.Semi, TokenKind.Bar, TokenKind.ThinArrow,
        TokenKind.DatumComment, TokenKind.Eof,
    ];

    private TokenKind? ReadTokenKind(Value v) => Payload(_tokenKind, v) switch
    {
        ("IdentTok", [var s]) when ReadStr(s) is { } name => new TokenKind.Ident(name),
        ("OperatorTok", [var s]) when ReadStr(s) is { } op => new TokenKind.Operator(op),
        ("IntTok", [Value.VAtom { Atom: Atom.I64 n }]) => new TokenKind.Int(n.Value),
        ("CharTok", [Value.VAtom { Atom: Atom.Char c }]) => new TokenKind.Char(c.Value),
        ("StringTok", [var s]) when ReadStr(s) is { } str => new TokenKind.Str(str),
        ("KeywordTok", [var s]) when ReadStr(s) is { } kw && TokenKind.Keywords.TryGetValue(kw, out var word) => word,
        ("PunctTok", [var s]) when ReadStr(s) is { } p => Punctuation.FirstOrDefault(w => w.Spelling == p),
        // `()` is a group here; a unit token has no reading.
        ("UnitTok", _) => throw new NotImplementedException("not ported yet: reading a reflected unit token"),
        _ => null,
    };

    public Fun.Kernel.TokenTree? ReadTokenTree(Value v) => Payload(TokenTreeType, v) switch
    {
        ("Tok", [var span, var kind, Value.VAtom { Atom: Atom.Scopes scopes }])
            when ReadSpan(span) is { } sp && ReadTokenKind(kind) is { } k
                 && (k is not TokenKind.Ident i || Certified(i.Name, scopes.ResolvedName))
            => new Fun.Kernel.TokenTree.Leaf(new Token(k, sp, scopes.Set)),
        ("TokGroup", [var span, var d, var items])
            when ReadSpan(span) is { } sp && ReadDelim(d) is { } delim && ReadList(items, ReadTokenTree) is { } ts
            => new Fun.Kernel.TokenTree.Group(delim, ts, sp),
        _ => null,
    };

    public EquatableArray<Fun.Kernel.TokenTree>? ReadTokens(Value v) => ReadList(v, ReadTokenTree);

    private Assoc? ReadAssoc(Value v) => Payload(_assoc, v) switch
    {
        ("Left", { IsEmpty: true }) => Assoc.Left,
        ("Right", { IsEmpty: true }) => Assoc.Right,
        ("NonAssoc", { IsEmpty: true }) => Assoc.None,
        _ => null,
    };

    private HoleKind? ReadHoleKind(Value v) => Payload(_holeKind, v) switch
    {
        ("HoleExpr", { IsEmpty: true }) => HoleKind.Expr,
        ("HoleBlock", { IsEmpty: true }) => HoleKind.Block,
        ("HoleId", { IsEmpty: true }) => HoleKind.Id,
        ("HoleDecl", { IsEmpty: true }) => HoleKind.Decls,
        ("HoleOneDecl", { IsEmpty: true }) => HoleKind.Decl,
        ("HolePattern", { IsEmpty: true }) => HoleKind.Pattern,
        ("HoleTokens", { IsEmpty: true }) => HoleKind.Tokens,
        _ => null,
    };

    public Syntax? ReadExpr(Value v)
    {
        if (Payload(ExprType, v) is not (var name, [var spanV, .. var args])) return null;
        if (ReadSpan(spanV) is not { } span) return null;
        Syntax? X(Value x) => ReadExpr(x);

        switch (name, args.Length)
        {
            case ("RawVar", 1): return ReadId(args[0]) is { } id ? new Syntax.Var(id) : null;
            case ("RawAtom", 1): return ReadAtom(args[0]) is { } a ? new Syntax.Atom(a, span) : null;
            case ("RawSelf", 0): return new Syntax.Self(span);
            case ("RawSelfType", 0): return new Syntax.SelfType(span);
            case ("RawAp", 3):
                return X(args[0]) is { } f && ReadExplicitness(args[1]) is { } apEx && X(args[2]) is { } arg
                    ? new Syntax.Ap(f, apEx, arg, span) : null;
            case ("RawLam", 2):
                return ReadParam(args[0]) is { } p && X(args[1]) is { } lamBody ? new Syntax.Lam(p, lamBody, span) : null;
            case ("RawLet", 5):
            {
                if (ReadId(args[0]) is not { } n || ReadOption(args[1], X) is not (true, var type)) return null;
                return X(args[2]) is { } value && X(args[3]) is { } body && ReadBool(args[4]) is { } rec
                    ? new Syntax.Let(n, type, value, body, rec, span) : null;
            }
            case ("RawLetRecGroup", 3):
            {
                if (ReadList(args[0], ReadId) is not { } names || ReadList(args[1], X) is not { } values || X(args[2]) is not { } body) return null;
                return names.Length == values.Length
                    ? new Syntax.LetRecGroup([.. names.Zip(values, (n, x) => new RecMember(n, x))], body, span) : null;
            }
            case ("RawAnnotated", 2):
                return X(args[0]) is { } inner && X(args[1]) is { } typ ? new Syntax.Annotated(inner, typ, span) : null;
            case ("RawProd", 1): return ReadList(args[0], X) is { } items ? new Syntax.Prod(items, span) : null;
            case ("RawProdTy", 1): return ReadList(args[0], X) is { } tys ? new Syntax.ProdTy(tys, span) : null;
            case ("RawTraitBoundSet", 1): return ReadList(args[0], X) is { } traits ? new Syntax.TraitBoundSet(traits, span) : null;
            case ("RawArrow", 5):
            {
                if (ReadExplicitness(args[0]) is not { } ex || ReadOption(args[1], ReadId) is not (true, var aname)) return null;
                if (X(args[2]) is not { } dom || ReadOption(args[3], ReadEffectRow) is not (true, var row) || X(args[4]) is not { } cod) return null;
                return new Syntax.Arrow(ex, aname, dom, row, cod, span);
            }
            case ("RawFieldAccess", 2):
                return X(args[0]) is { } of && ReadStr(args[1]) is { } field ? new Syntax.FieldAccess(of, field, span) : null;
            case ("RawProj", 2):
                return X(args[0]) is { } pOf && ReadI64(args[1]) is long idx ? new Syntax.Proj(pOf, (int)idx, span) : null;
            case ("RawRecordConstruct", 2):
                return X(args[0]) is { } rtyp && ReadFields(args[1]) is { } fields ? new Syntax.RecordConstruct(rtyp, fields, span) : null;
            case ("RawStruct", 1): return ReadDecls(args[0]) is { } sb ? new Syntax.Struct(sb, span) : null;
            case ("RawModule", 1): return ReadDecls(args[0]) is { } mb ? new Syntax.Module(mb, span) : null;
            case ("RawSig", 1): return ReadDecls(args[0]) is { } gb ? new Syntax.Sig(gb, span) : null;
            case ("RawEnum", 2):
            {
                if (ReadOption(args[0], ReadStr) is not (true, _)) return null;
                var ctors = ReadList(args[1], c => Payload(_ctor, c) is ("MkCtor", [var cid, var payloads])
                    && ReadId(cid) is { } ci && ReadList(payloads, X) is { } ps ? new EnumConstructor(ci.Name, ps) : null);
                return ctors is { } cs ? new Syntax.Enum(cs, span) : null;
            }
            case ("RawImport", 2):
                return ReadStr(args[0]) is { } path && args[1] is Value.VAtom { Atom: Atom.Scopes sc }
                    ? new Syntax.Import(path, span) { Scope = sc.Set } : null;
            case ("RawOpen", 3):
                return X(args[0]) is { } m && X(args[1]) is { } obody && ReadStr(args[2]) is { } label
                    ? new Syntax.Open(m, obody, label, span) : null;
            case ("RawOpenChoice", 3):
            {
                if (ReadId(args[0]) is not { } cname || ReadList(args[1], ReadStr) is not { } opens) return null;
                return ReadOption(args[2], ReadStr) is (true, var fallback) ? new Syntax.OpenChoice(cname, opens, fallback) : null;
            }
            case ("RawEffectDef", 4):
                return ReadId(args[0]) is { } ename && ReadList(args[1], ReadId) is { } eps
                       && ReadList(args[2], ReadEffectOp) is { } ops && X(args[3]) is { } ebody
                    ? new Syntax.EffectDef(ename, eps, ops, ebody, span) : null;
            case ("RawTraitDef", 4):
            {
                if (ReadId(args[0]) is not { } tname || ReadList(args[1], ReadId) is not { } tps
                    || ReadFields(args[2]) is not { } tfields || X(args[3]) is not { } tbody) return null;
                return tps.Length == 1
                    ? new Syntax.TraitDef(tname, tps[0], tfields, tbody, span)
                    : throw new NotImplementedException("not ported yet: a trait with other than one parameter");
            }
            case ("RawImplDef", 5):
            {
                if (ReadOption(args[0], ReadId) is not (true, var iname) || ReadPath(args[1]) is not { } trait
                    || ReadList(args[2], X) is not { } iargs || ReadFields(args[3]) is not { } ifields || X(args[4]) is not { } ibody) return null;
                return iargs.Length == 1
                    ? new Syntax.ImplDef(iname, trait, iargs[0], ifields, ibody, span)
                    : throw new NotImplementedException("not ported yet: an impl of other than one argument");
            }
            case ("RawPerform", 2):
                return ReadPath(args[0]) is Syntax.FieldAccess op && X(args[1]) is { } parg ? new Syntax.Perform(op, parg, span) : null;
            case ("RawResume", 1): return X(args[0]) is { } ra ? new Syntax.Resume(ra, span) : null;
            case ("RawRefNew", 1): return X(args[0]) is { } na ? new Syntax.RefNew(na, span) : null;
            case ("RawRefGet", 1): return X(args[0]) is { } ga ? new Syntax.RefGet(ga, span) : null;
            case ("RawRefSet", 2): return X(args[0]) is { } sr && X(args[1]) is { } sv ? new Syntax.RefSet(sr, sv, span) : null;
            case ("RawMatch", 2):
                return X(args[0]) is { } scrut && ReadList(args[1], ReadBranch) is { } branches ? new Syntax.Match(scrut, branches, span) : null;
            case ("RawStx", 1): return X(args[0]) is { } inner2 ? new Syntax.Stx(inner2, span) : null;
            case ("RawQuote", 2):
                return X(args[0]) is { } template && ReadQuoteHoles(args[1]) is { } holes ? new Syntax.Quote(template, holes, span) : null;
            case ("RawQuoteDecls", 2):
                return ReadDecls(args[0]) is { } qitems && ReadQuoteHoles(args[1]) is { } qholes ? new Syntax.QuoteDecls(qitems, qholes, span) : null;
            case ("RawMacroDef", 5):
            {
                if (ReadId(args[0]) is not { } mname || X(args[1]) is not { } mvalue || X(args[2]) is not { } mbody) return null;
                if (ReadOptionS(args[3], ReadAnn) is not (true, var kind) || ReadOption(args[4], X) is not (true, var output)) return null;
                return new Syntax.MacroDef(mname, mvalue, mbody, kind, output, span);
            }
            case ("RawSyntaxDef", 3):
                return ReadId(args[0]) is { } sname && ReadRole(args[1]) is { } role && X(args[2]) is { } sbody
                    ? new Syntax.SyntaxDef(sname, role, sbody, span) : null;
            case ("RawBlock", 1): return ReadTokens(args[0]) is { } ts ? new Syntax.Block(ts, span) : null;
            case ("RawInstantiate", 4):
                return ReadInstantiation(args[0], args[1], args[2], args[3]) is { } inst ? new Syntax.Instantiate(inst, span) : null;
            case ("RawMacroCall", 2):
                return X(args[0]) is { } head && ReadList(args[1], ReadCaptured) is { } cargs ? new Syntax.MacroCall(head, cargs, span) : null;
            case ("RawOperatorUse", 6):
            {
                if (ReadId(args[0]) is not { } operatorId || ReadFixity(args[1]) is not { } fx || ReadList(args[2], X) is not { } operands) return null;
                if (ReadSpan(args[3]) is not { } declared || ReadSpan(args[4]) is not { } used || ReadOption(args[5], ReadStr) is not (true, var unit)) return null;
                return new Syntax.OperatorUse(operatorId, fx, operands, declared, unit, used);
            }
            case ("RawTypeDef", _):
                throw new NotImplementedException($"not ported yet: reading the reflected form {name}");
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
        if (Payload(_role, v) is not ("MkRole", [var fixity, var order, var meaning, var declared, var fromUnit])) return null;
        if (ReadFixity(fixity) is not { } fx || ReadOption(order, ReadOrder) is not (true, var ord)) return null;
        RoleMeaning? m = Payload(_roleMeaning, meaning) switch
        {
            ("ApplyValue", { IsEmpty: true }) => RoleMeaning.ApplyValue.Instance,
            ("AssignRef", { IsEmpty: true }) => RoleMeaning.AssignRef.Instance,
            ("CallMacro", { IsEmpty: true }) => RoleMeaning.CallMacro.Instance,
            ("Rules", [var kind, var rules]) when ReadAnn(kind) is { } k && ReadList(rules, ReadRule) is { } rs => new RoleMeaning.Rules(k, rs),
            ("OrderGroup", { IsEmpty: true }) => RoleMeaning.OrderGroup.Instance,
            ("PolyArrow", { IsEmpty: true }) => RoleMeaning.PolyArrow.Instance,
            _ => null,
        };
        if (m is null || ReadSpan(declared) is not { } at) return null;
        return ReadOption(fromUnit, ReadStr) is (true, var unit) ? new Role(fx, ord, m, at, unit) : null;
    }

    private Order? ReadOrder(Value v) => Payload(_order, v) is ("MkOrder", [var g, var n, var a, var w, var st, var wk])
        && ReadStr(g) is { } group && ReadStr(n) is { } name && ReadAssoc(a) is { } assoc && ReadBool(w) is { } weakest
        && ReadList(st, ReadOrder) is { } stronger && ReadList(wk, ReadOrder) is { } weaker
            ? new Order(group, name, assoc, weakest, stronger, weaker)
            : null;

    private Rule? ReadRule(Value v)
    {
        if (Payload(_rule, v) is not ("MkRule", [var pattern, var replacement, var span])) return null;
        if (ReadList(pattern, ReadRulePart) is not { } parts || ReadSpan(span) is not { } sp) return null;
        Replacement? r = Payload(_replacement, replacement) switch
        {
            ("ReplaceExpr", [var e]) when ReadExpr(e) is { } x => new Replacement.Expr(x),
            ("ReplaceDecls", [var ds]) when ReadDecls(ds) is { } bs => new Replacement.Decls(bs),
            _ => null,
        };
        return r is null ? null : new Rule(parts, r, sp);
    }

    private RulePart? ReadRulePart(Value v) => Payload(_rulePart, v) switch
    {
        ("PartToken", [var t]) when ReadTokenTree(t) is { } tree => new RulePart.Literal(tree),
        ("PartGroup", [var d, var parts, var span]) when ReadDelim(d) is { } delim && ReadList(parts, ReadRulePart) is { } ps && ReadSpan(span) is { } sp
            => new RulePart.Group(delim, ps, sp),
        ("PartHole", [var hole, var kind, var span]) when ReadStr(hole) is { } h && ReadHoleKind(kind) is { } k && ReadSpan(span) is { } sp
            => new RulePart.Hole(h, k, sp),
        _ => null,
    };

    private Capture? ReadCaptured(Value v) => Payload(_captured, v) switch
    {
        ("CapExpr", [var e]) when ReadExpr(e) is { } x => new Capture.Expr(x),
        ("CapBlock", [var ts]) when ReadTokens(ts) is { } terms => new Capture.Block(terms),
        ("CapId", [var t]) when ReadTokenTree(t) is Fun.Kernel.TokenTree.Leaf leaf => new Capture.Id(leaf.Token),
        ("CapPattern", [var p]) when ReadPattern(p) is { } pat => new Capture.Pattern(pat),
        ("CapDecls", [var ds]) when ReadDecls(ds) is { } bs => new Capture.Decls(bs),
        ("CapDecl", [var d]) when ReadDecl(d) is { } b => new Capture.Decl(b),
        ("CapTokens", [var ts]) when ReadTokens(ts) is { } terms => new Capture.Tokens(terms),
        _ => null,
    };

    private EquatableArray<(string Hole, Capture Capture)>? ReadCaptures(Value v) =>
        ReadListS(v, c => Payload(_capture, c) is ("MkCapture", [var n, var captured]) && ReadStr(n) is { } name && ReadCaptured(captured) is { } cap
            ? (name, cap) : ((string, Capture)?)null);

    private EquatableArray<(string Hole, Syntax Value)>? ReadQuoteHoles(Value v) =>
        ReadListS(v, h => Payload(_quoteHole, h) is ("MkQuoteHole", [var n, var e]) && ReadStr(n) is { } name && ReadExpr(e) is { } x
            ? (name, x) : ((string, Syntax)?)null);

    private EquatableArray<(string Name, Syntax Value)>? ReadFields(Value v) =>
        ReadListS(v, f => Payload(_field, f) is ("MkField", [var n, var e]) && ReadStr(n) is { } name && ReadExpr(e) is { } x
            ? (name, x) : ((string, Syntax)?)null);

    private Param? ReadParam(Value v)
    {
        if (Payload(_param, v) is not ("MkParam", [var n, var ty, var bounds, var ex])) return null;
        if (ReadId(n) is not { } name || ReadOption(ty, ReadExpr) is not (true, var type) || ReadExplicitness(ex) is not { } e) return null;
        if (ReadList(bounds, ReadPath) is not { } bs) return null;
        return bs.IsEmpty ? new Param(name, type, e) : throw new NotImplementedException("not ported yet: reading reflected trait bound paths");
    }

    private EffectRow? ReadEffectRow(Value v) => Payload(_effectRow, v) is ("MkEffectRow", [var effects, var tails, var inferred, var poly])
        && ReadList(effects, ReadExpr) is { } es && ReadList(tails, ReadExpr) is { } ts && ReadBool(inferred) is { } inf && ReadBool(poly) is { } p
            ? new EffectRow(es, ts, inf, p)
            : null;

    private EffectOp? ReadEffectOp(Value v) => Payload(_effectOp, v) is ("MkEffectOp", [var n, var input, var output])
        && ReadStr(n) is { } name && ReadExpr(input) is { } i && ReadExpr(output) is { } o
            ? new EffectOp(name, i, o)
            : null;

    private MatchBranch? ReadBranch(Value v) => Payload(_branch, v) switch
    {
        ("ValueBranch", [var p, var body]) when ReadPattern(p) is { } pat && ReadExpr(body) is { } b => new MatchBranch(pat, b),
        ("EffectBranch", [var op, var p, var body]) when ReadPath(op) is Syntax.FieldAccess path && ReadPattern(p) is { } pat && ReadExpr(body) is { } b
            => new MatchBranch(pat, b) { Operation = path },
        _ => null,
    };

    public Fun.Kernel.Pattern? ReadPattern(Value v)
    {
        if (Payload(PatternType, v) is not (var name, [var spanV, .. var args]) || ReadOption(spanV, x => Nbe.Force(_metas, x)) is not (true, _)) return null;
        (string, Fun.Kernel.Pattern)? PatField(Value f) => Payload(_patField, f) is ("MkPatField", [var n, var p]) && ReadStr(n) is { } fname
            && ReadOption(p, ReadPattern) is (true, var fp)
                // `{y}` is `{y = y}`: a binder written by the label.
                ? (fname, fp ?? new Fun.Kernel.Pattern.Bind(new Id(fname, SourceSpan.Synthetic)))
                : null;
        return (name, args.Length) switch
        {
            ("RawPatWild", 0) => Fun.Kernel.Pattern.Wild.Instance,
            ("RawPatBind", 1) => ReadId(args[0]) is { } id ? new Fun.Kernel.Pattern.Bind(id) : null,
            ("RawPatCon", 2) => ReadPath(args[0]) is { } head && ReadList(args[1], ReadPattern) is { } ps ? new Fun.Kernel.Pattern.Con(head, ps) : null,
            ("RawPatAtom", 1) => ReadAtom(args[0]) is { } a ? new Fun.Kernel.Pattern.Atom(a) : null,
            ("RawPatProd", 1) => ReadList(args[0], ReadPattern) is { } items ? new Fun.Kernel.Pattern.Prod(items) : null,
            ("RawPatOr", 2) => ReadPattern(args[0]) is { } l && ReadPattern(args[1]) is { } r ? new Fun.Kernel.Pattern.Or(l, r) : null,
            ("RawPatRecord", 3) => ReadPath(args[0]) is { } typ && ReadListS(args[1], PatField) is { } fs && ReadBool(args[2]) is { } partial
                ? new Fun.Kernel.Pattern.Record(typ, fs, partial) : null,
            ("RawPatStructType", 2) => ReadListS(args[0], PatField) is { } sfs && ReadBool(args[1]) is { } spartial
                ? new Fun.Kernel.Pattern.StructType(sfs, spartial) : null,
            ("RawPatType", 1) => ReadAtomTy(args[0]) is { } t ? new Fun.Kernel.Pattern.AtomType(t) : null,
            _ => null,
        };
    }

    public Binding? ReadDecl(Value v)
    {
        if (Payload(DeclType, v) is not (var name, var args)) return null;
        Syntax? X(Value x) => ReadExpr(x);
        switch (name, args.Length)
        {
            case ("DeclLet", 4):
                return ReadId(args[0]) is { } n && X(args[1]) is { } value && ReadBool(args[2]) is { } pub && ReadBool(args[3]) is { } rec
                    ? new Binding.Let(n, value, pub, rec) : null;
            case ("DeclRecGroup", 3):
            {
                if (ReadList(args[0], ReadId) is not { } names || ReadList(args[1], X) is not { } values || ReadBool(args[2]) is not { } gpub) return null;
                return names.Length == values.Length ? new Binding.RecGroup([.. names.Zip(values, (a, b) => new RecMember(a, b))], gpub) : null;
            }
            case ("DeclMethod", 5):
            {
                if (ReadId(args[0]) is not { } mn || ReadList(args[1], ReadParam) is not { } ps) return null;
                if (ReadOption(args[2], ReadEffectRow) is not (true, var row) || X(args[3]) is not { } body || ReadBool(args[4]) is not { } mpub) return null;
                return new Binding.Method(mn, ps, body, mpub, row);
            }
            case ("DeclEffect", 4):
                return ReadId(args[0]) is { } en && ReadList(args[1], ReadId) is { } eps && ReadList(args[2], ReadEffectOp) is { } ops && ReadBool(args[3]) is { } epub
                    ? new Binding.Effect(en, eps, ops, epub) : null;
            case ("DeclTrait", 4):
            {
                if (ReadId(args[0]) is not { } tn || ReadList(args[1], ReadId) is not { } tps || ReadFields(args[2]) is not { } tf || ReadBool(args[3]) is not { } tpub) return null;
                return tps.Length == 1 ? new Binding.Trait(tn, tps[0], tf, tpub) : throw new NotImplementedException("not ported yet: a trait with other than one parameter");
            }
            case ("DeclImpl", 5):
            {
                if (ReadOption(args[0], ReadId) is not (true, var iname) || ReadPath(args[1]) is not { } trait || ReadList(args[2], X) is not { } iargs
                    || ReadFields(args[3]) is not { } ifields || ReadBool(args[4]) is not { } ipub) return null;
                return iargs.Length == 1 ? new Binding.Impl(iname, trait, iargs[0], ifields, ipub) : throw new NotImplementedException("not ported yet: an impl of other than one argument");
            }
            case ("DeclMacro", 5):
            {
                if (ReadId(args[0]) is not { } man || X(args[1]) is not { } mvalue || ReadBool(args[2]) is not { } mapub) return null;
                if (ReadOptionS(args[3], ReadAnn) is not (true, var kind) || ReadOption(args[4], X) is not (true, var output)) return null;
                return new Binding.Macro(man, mvalue, mapub, kind, output);
            }
            case ("DeclMacroCall", 3):
                return X(args[0]) is { } head && ReadList(args[1], ReadCaptured) is { } cargs && ReadBool(args[2]) is { } cpub
                    ? new Binding.MacroCall(head, cargs, cpub) : null;
            case ("DeclPatternSyn", 4):
                return ReadId(args[0]) is { } sn && ReadList(args[1], ReadId) is { } sps && ReadPattern(args[2]) is { } rhs && ReadBool(args[3]) is { } spub
                    ? new Binding.Let(sn, new Syntax.PatternSynonym(sps, rhs, sn.Span), spub, false) : null;
            case ("DeclField", 2):
                return ReadStr(args[0]) is { } fname && X(args[1]) is { } ftype ? new Binding.Field(fname, ftype) : null;
            case ("DeclOpen", 2):
                return X(args[0]) is { } of && ReadStr(args[1]) is { } label ? new Binding.Open(of, label) : null;
            case ("DeclExport", 3):
            {
                if (X(args[0]) is not { } eof) return null;
                var (namesOk, exported) = ReadOption(args[1], n => ReadList(n, ReadStr) is { } list ? new Boxed<EquatableArray<string>>(list) : null);
                return namesOk && ReadBool(args[2]) is { } xpub ? new Binding.Export(eof, exported?.Value, xpub) : null;
            }
            case ("DeclHole", 1): return ReadId(args[0]) is { } hid ? new Binding.Hole(hid) : null;
            case ("DeclSyntax", 3):
                return ReadId(args[0]) is { } sname && ReadRole(args[1]) is { } role && ReadBool(args[2]) is { } sypub
                    ? new Binding.SyntaxDecl(sname, role, sypub) : null;
            case ("DeclItems", 1): return ReadTokens(args[0]) is { } ts ? new Binding.Items(ts) : null;
            case ("DeclInstantiate", 5):
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
