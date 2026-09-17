using Fun.Kernel;

namespace Fun.Expand;

public sealed partial class Expander
{
    /// <summary>Every compiled macro this expansion defined or imported, by the resolved name its binder carries.</summary>
    private Dictionary<string, MacroEntry> _macros = [];

    /// <summary>Macros whose definition is being expanded: a call to one is an error, not a lookup miss.</summary>
    private HashSet<string> _provisional = [];

    /// <summary>
    /// While a macro's definition is expanded, the first scope minted for it: quoted
    /// syntax is pruned of every scope from here on.
    /// </summary>
    private int? _macroDefinitionFloor;

    /// <summary>The public macros this expansion declared: a unit's macro exports.</summary>
    private readonly List<(string Name, MacroEntry Macro)> _macroExports = [];

    /// <summary>A compiled macro, by the resolved name a typed call's head carries.</summary>
    public MacroEntry? LookupMacro(string resolved) => _macros.GetValueOrDefault(resolved);

    // ---- definitions ----------------------------------------------------------

    /// <summary>
    /// A macro definition: compiled where it is defined, then bound for the forms
    /// after it (its own body sees the binder only as provisional). Returns the
    /// scope those forms carry.
    /// </summary>
    private ScopeSet DefineMacro(Id name, Syntax value, FormKind? kind, Syntax? output, bool isPublic)
    {
        var position = kind == FormKind.Decl ? FormKind.Decl : FormKind.Expr;
        var (compiled, signature) = Compiled(value, kind, output);
        var (kinds, withKinds) = ParameterKinds(compiled);

        // A macro is a syntactic role's sort of binder (M7): it mixes with values, not roles.
        CheckRoleMixing(name.Name, name.Scope, isRole: true, attaches: false, group: false);
        var scope = FreshScope();
        var resolved = FreshResolvedName(name.Name);
        _bindings.Extend(name.Name, name.Scope.Union(scope), resolved, BinderMeaning.Macro, macroParams: kinds);

        _provisional.Add(resolved);
        try
        {
            var definition = InMacroDefinition(() => Expand(withKinds));
            var fn = _runtime.CompileMacro(InDefinitionSiteOpens(name, definition));
            var compiledSignature = signature is null ? null : new CompiledSignature(
                _runtime.CompileSignature(InDefinitionSiteOpens(name, InMacroDefinition(() => Expand(signature.Type)))),
                signature.Binders, signature.Params);
            var entry = new MacroEntry(fn, position, kinds, compiledSignature);
            _macros[resolved] = entry;
            if (isPublic) _macroExports.Add((name.Name, entry));
        }
        finally
        {
            _provisional.Remove(resolved);
        }
        return scope;
    }

    /// <summary>
    /// A macro's value as it is compiled, and its signature. A <c>: Decl</c> macro's
    /// output is the type its body returns, so its body is checked against it; it has
    /// no signature, and runs during expansion.
    /// </summary>
    private static (Syntax Value, MacroSignature? Signature) Compiled(Syntax value, FormKind? kind, Syntax? output)
    {
        if (kind != FormKind.Decl) return (value, SignatureOf(output, value));
        if (output is null) return (value, null);
        Syntax Annotate(Syntax s) => s is Syntax.Lam l ? l with { Body = Annotate(l.Body) } : new Syntax.Annotated(s, output, s.Span);
        return (Annotate(value), null);
    }

    /// <summary>The <c>T</c> of a parameter written <c>(x : Expr(T))</c>.</summary>
    private static Syntax? TypedExprParam(Param p) =>
        p.Type is Syntax.Ap { Fn: Syntax.Var { Id.Name: "Expr" }, Explicitness: Explicitness.Explicit, Arg: var t } ? t : null;

    /// <summary>
    /// A macro's signature (macro-annotation-constraints-mean-nothing): its type binders,
    /// then the <c>T</c> of each <c>(x : Expr(T))</c> parameter, then the <c>T</c> its
    /// <c>: Expr(T)</c> output promises; an output that promises nothing is one more type
    /// binder, solved at the call. Null when the macro promises no type: it runs during
    /// expansion.
    /// </summary>
    private static MacroSignature? SignatureOf(Syntax? output, Syntax value)
    {
        var parameters = new List<Param>();
        for (var s = value; s is Syntax.Lam l; s = l.Body) parameters.Add(l.Param);
        var binders = parameters.Where(p => p.Explicitness == Explicitness.Implicit).ToList();
        var explicits = parameters.Where(p => p.Explicitness == Explicitness.Explicit).ToList();
        EquatableArray<(string, bool)> paramKinds = [.. explicits.Select(p => (p.Name.Name, TypedExprParam(p) is not null))];
        if (binders.Count == 0 && output is null && !paramKinds.Any(p => p.Item2)) return null;

        var span = value.Span;
        Syntax TypeAt(Id id) => new Syntax.Var(id with { Name = "Type" });
        Syntax Pi(Explicitness e, Id? name, Syntax domain, Syntax codomain) => new Syntax.Arrow(e, name, domain, null, codomain, span);

        // `$` cannot begin a source identifier, so no annotation can mention it.
        var resultBinder = output is null ? new Id("$output", span) : null;
        Syntax result = output ?? new Syntax.Var(resultBinder!);
        var codomain = explicits.AsEnumerable().Reverse().Aggregate(result,
            (acc, p) => TypedExprParam(p) is { } t ? Pi(Explicitness.Explicit, null, t, acc) : acc);
        if (resultBinder is not null) codomain = Pi(Explicitness.Implicit, resultBinder, TypeAt(resultBinder), codomain);
        var pis = binders.AsEnumerable().Reverse().Aggregate(codomain, (acc, p) => Pi(Explicitness.Implicit, p.Name, TypeAt(p.Name), acc));
        // Annotated as a type, so a promised T that is not one is an error.
        return new MacroSignature(new Syntax.Annotated(pis, TypeAt(new Id("Type", span)), span),
            [.. binders.Select(p => p.Name.Name)], paramKinds);
    }

    /// <summary>
    /// A macro's parameter kinds (M9): each explicit parameter annotated with a kind --
    /// <c>(n : Id)</c> -- takes that kind, any other an <c>Expr</c>. The kind is the
    /// parameter's type, the reflection type in the <c>Syntax</c> module found from the
    /// annotation's own scopes: a <c>Block</c> is the <c>Expr</c> it stands as, a
    /// <c>Decl</c> one declaration, <c>List(Decl)</c> a brace group of any number.
    /// </summary>
    private static (EquatableArray<HoleKind>, Syntax) ParameterKinds(Syntax value)
    {
        Syntax SyntaxMember(Id written, string member, SourceSpan span) =>
            new Syntax.FieldAccess(new Syntax.Var(written with { Name = "Syntax" }), member, span);

        (HoleKind, Syntax?) KindType(Param p) => p.Type switch
        {
            // (x : Expr(T)): an Expr whose type the macro's signature promises.
            Syntax.Ap { Fn: Syntax.Var { Id: { Name: "Expr" } written }, Explicitness: Explicitness.Explicit } ty
                => (HoleKind.Expr, SyntaxMember(written, "Expr", ty.Span)),
            Syntax.Ap { Fn: Syntax.Var { Id.Name: "List" }, Explicitness: Explicitness.Explicit, Arg: Syntax.Var { Id: { Name: "Decl" } decl } } ty
                => (HoleKind.Decls, SyntaxMember(decl, "Decls", ty.Span)),
            Syntax.Ap { Fn: Syntax.Var { Id.Name: "List" } list, Explicitness: Explicitness.Explicit, Arg: Syntax.Var { Id: { Name: "TokenTree" } tree } } ty
                => (HoleKind.Tokens, ty with { Arg = SyntaxMember(tree, "TokenTree", tree.Span) }),
            Syntax.Var { Id: var written } ty when KindOfName(written.Name) is { } kind
                => (kind, SyntaxMember(written, kind switch
                {
                    HoleKind.Block or HoleKind.Expr => "Expr",
                    HoleKind.Id => "Id",
                    HoleKind.Decl => "Decl",
                    _ => "Pattern",
                }, ty.Span)),
            _ => (HoleKind.Expr, p.Type),
        };

        var kinds = new List<HoleKind>();
        Syntax Go(Syntax s)
        {
            if (s is not Syntax.Lam l) return s;
            if (l.Param.Explicitness == Explicitness.Implicit) return l with { Body = Go(l.Body) };
            var (kind, type) = KindType(l.Param);
            kinds.Add(kind);
            return l with { Param = l.Param with { Type = type }, Body = Go(l.Body) };
        }
        var withKinds = Go(value);
        return ([.. kinds], withKinds);
    }

    private static HoleKind? KindOfName(string name) => name switch
    {
        "Expr" => HoleKind.Expr,
        "Block" => HoleKind.Block,
        "Id" => HoleKind.Id,
        "Decl" => HoleKind.Decl,
        "Pattern" => HoleKind.Pattern,
        _ => null,
    };

    /// <summary>
    /// Expands a macro's definition. Quoted syntax in it keeps the scopes of where the
    /// macro was defined, but not those of the binding forms inside the macro itself --
    /// its parameters and local lets -- which do not exist where its output lands
    /// (Flatt 2016, quote-syntax pruning).
    /// </summary>
    private T InMacroDefinition<T>(Func<T> expand)
    {
        var saved = _macroDefinitionFloor;
        _macroDefinitionFloor = _scopeCounter;
        try
        {
            return expand();
        }
        finally
        {
            _macroDefinitionFloor = saved;
        }
    }

    private SyntaxMapper PruneToDefinitionSite() => _macroDefinitionFloor is int floor
        ? SyntaxMapper.OfIds(id => id with { Scope = id.Scope.Where(s => s < floor) })
        : SyntaxMapper.OfIds(id => id);

    /// <summary>
    /// A macro body is compiled in its definition site's scope, and nothing is ambient
    /// there (macro-bodies-implicitly-open-the-prelude): the units opened around the
    /// definition, outermost outside, and no local binder, which has no value while
    /// the program is being expanded.
    /// </summary>
    private Syntax InDefinitionSiteOpens(Id name, Syntax body)
    {
        const string unit = "unit:";
        foreach (var (scope, label) in _opens.Where(o => o.Label.StartsWith(unit) && name.Scope.Contains(o.Scope)).OrderByDescending(o => o.Scope))
            body = new Syntax.Open(new Syntax.Import(label[unit.Length..], body.Span), body, label, body.Span);
        return body;
    }

    // ---- calls ------------------------------------------------------------------

    /// <summary>
    /// The macro a head names, by its key: a binder's resolved name; <c>M.m</c> where
    /// <c>M</c> denotes a unit exporting <c>m</c>; or, for an id no binder took that a
    /// form imported from a unit introduced, that unit's macro of its name.
    /// </summary>
    private string? MacroKey(Syntax head)
    {
        switch (head)
        {
            case Syntax.Var { Id.Name: var n } when n.Contains('#'):
                return _macros.ContainsKey(n) || _provisional.Contains(n) || UnitMacroOfKey(n) ? n : null;
            case Syntax.Var { Id: var id }:
                if (_bindings.Resolve(id) is { } binder)
                    return binder.Kind == BinderMeaning.Macro && (_macros.ContainsKey(binder.ResolvedName) || _provisional.Contains(binder.ResolvedName) || UnitMacroOfKey(binder.ResolvedName)) ? binder.ResolvedName : null;
                foreach (var scope in id.Scope.Values)
                    if (_introScopeUnits.TryGetValue(scope, out var unit) && UnitMacro(unit, id.Name) is not null)
                        return UnitMacroKey(unit, id.Name);
                return null;
            case Syntax.FieldAccess { Of: var of, Field: var name } when UnitPathOf(of) is { } path:
                return UnitMacro(path, name) is not null ? UnitMacroKey(path, name) : null;
            default:
                return null;
        }
    }

    /// <summary>Whether <paramref name="key"/> is a unit macro's key, registering the macro when it is.</summary>
    private bool UnitMacroOfKey(string key) =>
        key.IndexOf("#unit:", StringComparison.Ordinal) is var at and >= 0 && UnitMacro(key[(at + "#unit:".Length)..], key[..at]) is not null;

    private MacroEntry EntryFor(string key, Syntax head, FormKind position, int argumentCount)
    {
        var written = Label(key);
        if (_provisional.Contains(key)) throw new ExpandException($"macro {written} is expanded during its own definition");
        var entry = _macros.GetValueOrDefault(key) ?? throw new InvalidOperationException($"a macro binder with no compiled macro: {key}");
        // M8: a macro's kind must match the position it is used in; checked before it runs.
        if (entry.Position != position)
            throw new ExpandException($"macro {written} returns {(entry.Position == FormKind.Decl ? "declarations" : "an expression")} but is used where {(position == FormKind.Decl ? "declarations go" : "an expression goes")}");
        if (argumentCount != entry.Params.Length)
            throw new ExpandException($"macro {written} takes {entry.Params.Length} arguments, the call gives {argumentCount}");
        return entry;
    }

    private static string Label(string resolved) => resolved.IndexOf('#') is var i and >= 0 ? resolved[..i] : resolved;

    private MacroExpansion Expansion() => new(Expand, ExpandDeclsApart);

    /// <summary>
    /// A call the elaborator applies (a macro whose signature promises types): one
    /// application's hygiene around <paramref name="run"/>, which runs the macro on the
    /// arguments as received; returns its output expanded in place (M6).
    /// </summary>
    public Syntax ApplyTyped(EquatableArray<Capture> args, Func<EquatableArray<Capture>, MacroExpansion, Syntax> run)
    {
        var app = NewApplication(null);
        return Expand(run([.. args.Select(app.Receive.MapCapture)], Expansion()).Map(app.Emit));
    }

    /// <summary>
    /// A macro's call in expression position: applied now, its output expanded in place
    /// (M6) -- or, for a macro whose signature promises types, left for the elaborator,
    /// its expression arguments travelling as syntax.
    /// </summary>
    private Syntax ExpandMacroCall(Syntax.MacroCall call)
    {
        if (MacroKey(call.Head) is not { } key)
            throw new ExpandException($"`{(call.Head as Syntax.Var)?.Id.Name}` is not a macro");
        var entry = EntryFor(key, call.Head, FormKind.Expr, call.Args.Length);
        if (entry.Signature is not null)
            return call with
            {
                Head = new Syntax.Var(new Id(key, call.Head.Span)),
                Args = [.. call.Args.Select(a => a is Capture.Expr e ? new Capture.Expr(new Syntax.Stx(e.Syntax, e.Syntax.Span)) : a)],
            };

        var app = NewApplication(null);
        var output = _runtime.ApplyExpr(Label(key), entry, [.. call.Args.Select(app.Receive.MapCapture)], Expansion());
        return Expand(output.Map(app.Emit));
    }

    /// <summary>
    /// A use of an operator whose role calls a macro: the macro of the operator's name,
    /// applied now and its output expanded in place (M6). A prefix use hands the macro
    /// its operand, an infix use its two operands when the macro takes two, and
    /// otherwise the whole use as syntax.
    /// </summary>
    private Syntax ExpandOperatorUse(Syntax.OperatorUse use)
    {
        var head = new Syntax.Var(use.Operator);
        if (MacroKey(head) is not { } key)
            throw new ExpandException($"`{use.Operator.Name}` is an operator macro with no macro of its name");
        var arity = _macros.GetValueOrDefault(key)?.Params.Length ?? 0;
        EquatableArray<Syntax> operands = use.Operands switch
        {
            [var only] => [only],
            [_, _] when arity >= 2 => use.Operands,
            _ => [use],
        };
        var entry = EntryFor(key, head, FormKind.Expr, operands.Length);
        if (entry.Signature is not null)
            throw new NotImplementedException($"not ported yet: a type-aware operator macro `{use.Operator.Name}`");

        var app = NewApplication(null);
        var output = _runtime.ApplyExpr(Label(key), entry, [.. operands.Select(o => app.Receive.MapCapture(new Capture.Expr(o)))], Expansion());
        return Expand(output.Map(app.Emit));
    }

    /// <summary>
    /// An application whose head names a macro, written as a curried call: the macro
    /// takes as many arguments as it declares, and the rest apply to its output.
    /// </summary>
    private Syntax? ExpandMacroApplication(Syntax.Ap ap)
    {
        var spine = new List<Syntax.Ap>();
        Syntax head = ap;
        while (head is Syntax.Ap a)
        {
            spine.Insert(0, a);
            head = a.Fn;
        }
        if (MacroKey(head) is not { } key) return null;

        var arity = _macros.GetValueOrDefault(key)?.Params.Length ?? 0;
        var taken = spine.Take(arity).ToList();
        var call = new Syntax.MacroCall(head, [.. taken.Select(a => (Capture)new Capture.Expr(a.Arg))], taken.LastOrDefault()?.Span ?? head.Span);
        Syntax result = ExpandMacroCall(call);
        foreach (var rest in spine.Skip(arity)) result = rest with { Fn = result, Arg = Expand(rest.Arg) };
        return result;
    }

    /// <summary>A declaration macro's call: the declarations it returns, ready to be bound where it was written.</summary>
    private EquatableArray<Binding> ApplyDeclMacro(Binding.MacroCall call)
    {
        if (MacroKey(call.Head) is not { } key)
            throw new ExpandException($"`{(call.Head as Syntax.Var)?.Id.Name}` is not a declaration macro");
        var entry = EntryFor(key, call.Head, FormKind.Decl, call.Args.Length);
        var app = NewApplication(null);
        var output = _runtime.ApplyDecls(Label(key), entry, [.. call.Args.Select(app.Receive.MapCapture)], Expansion());
        return [.. output.Select(b => EmitBinding(app, b))];
    }

    /// <summary>
    /// A declaration an application returns into a definition context binds for the
    /// rest of it, so its binders lose the use-site scope (Flatt 2016); unread items do
    /// too, on every token, and so do the unread tokens a declaration macro call is handed.
    /// </summary>
    private static Binding EmitBinding(Application app, Binding binding) => binding.Map(app.Emit) switch
    {
        Binding.Items items => items.Map(app.PruneUseSite),
        Binding.MacroCall call => call with
        {
            Args = [.. call.Args.Select(a => a is Capture.Tokens t ? app.PruneUseSite.MapCapture(t) : a)],
        },
        var flipped => flipped.MapBinders(app.PruneUseSite.Id),
    };

    /// <summary>
    /// <c>expand_decls</c>: a declaration list expands as a definition context of its
    /// own, so what it binds stays inside the result.
    /// </summary>
    private EquatableArray<Binding> ExpandDeclsApart(EquatableArray<Binding> decls)
    {
        var (bindings, macros, provisional, opens, exports, macroExports) =
            (_bindings, _macros, _provisional, _opens.ToList(), _syntaxExports.Count, _macroExports.Count);
        _bindings = _bindings.Copy();
        _macros = new(_macros);
        _provisional = [.. _provisional];
        try
        {
            return ExpandBindings(decls);
        }
        finally
        {
            (_bindings, _macros, _provisional) = (bindings, macros, provisional);
            _opens.Clear();
            _opens.AddRange(opens);
            _syntaxExports.RemoveRange(exports, _syntaxExports.Count - exports);
            _macroExports.RemoveRange(macroExports, _macroExports.Count - macroExports);
        }
    }

    // ---- quoted syntax ----------------------------------------------------------

    /// <summary>The template is data: nothing in it is resolved or renamed, only pruned to its definition site.</summary>
    private Syntax ExpandQuote(Syntax quote) => quote switch
    {
        Syntax.Quote q => q with { Template = q.Template.Map(PruneToDefinitionSite()), Holes = [.. q.Holes.Select(h => (h.Hole, Expand(h.Value)))] },
        Syntax.QuoteDecls q => q with
        {
            Items = [.. q.Items.Select(b => b.Map(PruneToDefinitionSite()))],
            Holes = [.. q.Holes.Select(h => (h.Hole, Expand(h.Value)))],
        },
        _ => throw new InvalidOperationException($"not a quote: {quote.GetType().Name}"),
    };
}
