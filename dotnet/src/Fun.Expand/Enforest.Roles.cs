using Fun.Kernel;

namespace Fun.Expand;

/// <summary>
/// The roles forms are read with, and how. Reading as expansion reaches a form
/// uses the expander's binder table; reading quoted syntax (a rule's
/// replacement) reads it completely where it is written, against a copy of the
/// table, registering each role it declares for the statements after it (M10).
/// </summary>
public sealed class EnforestEnv(
    BinderTable roles, Func<Syntax, EquatableArray<(string Name, Role Role)>?> unitRoles,
    bool eager, bool registers, EquatableArray<string> holes)
{
    public BinderTable Roles { get; } = roles;

    /// <summary>The roles the unit an expression denotes exports; null when it denotes no unit.</summary>
    public Func<Syntax, EquatableArray<(string Name, Role Role)>?> UnitRoles { get; } = unitRoles;

    /// <summary>Quoted syntax: blocks and module items are read now, not left for expansion.</summary>
    public bool Eager { get; } = eager;

    /// <summary>A role declared while reading is registered as it is read.</summary>
    public bool Registers { get; } = registers;

    /// <summary>The captures of the rules enclosing quoted syntax, which a rule declared in it may use.</summary>
    public EquatableArray<string> Holes { get; } = holes;

    /// <summary>Roles registered while reading, so a statement that declared one scopes it over the statements after it.</summary>
    public int Declared { get; set; }

    public static EnforestEnv Lazy(BinderTable roles, Func<Syntax, EquatableArray<(string Name, Role Role)>?> unitRoles) =>
        new(roles, unitRoles, eager: false, registers: false, []);

    public EnforestEnv Quoted(IEnumerable<string> holes) =>
        Eager
            ? new(Roles, UnitRoles, true, true, [.. holes, .. Holes]) { Declared = Declared }
            : new(Roles.Copy(), UnitRoles, true, true, [.. holes]);

    /// <summary>A struct's items are read together, each seeing the roles declared before it.</summary>
    public EnforestEnv RegisteringItems() => Registers ? this : new(Roles.Copy(), UnitRoles, Eager, true, Holes);
}

/// <summary>
/// Enforestation reads with an environment: the roles in scope, and whether it is
/// reading quoted syntax now or leaving bodies for expansion. Reading with another
/// environment is reading with another enforester, never a swapped ambient one.
/// </summary>
public sealed partial class Enforest(EnforestEnv env)
{
    private readonly EnforestEnv _env = env;

    /// <summary>An enforester reading with the roles of <paramref name="roles"/>, as expansion reaches each form.</summary>
    public static Enforest Lazy(BinderTable roles, Func<Syntax, EquatableArray<(string Name, Role Role)>?> unitRoles) =>
        new(EnforestEnv.Lazy(roles, unitRoles));

    // Scopes the enforester mints for the statements of quoted syntax count
    // down from -1, apart from the expander's.
    private static int _quotedScope;

    private static ScopeSet FreshQuotedScope() => ScopeSet.Singleton(Interlocked.Decrement(ref _quotedScope));

    private static int _orderCounter;

    // ---- precedence ---------------------------------------------------------

    /// <summary>
    /// Whether infix operator <paramref name="symbol"/> continues an expression
    /// read at <paramref name="prec"/>: at an operand, only if it binds tighter
    /// than the operator the operand belongs to -- by their groups' declared
    /// order, never a guess.
    /// </summary>
    private bool Continues(Prec prec, string symbol, Role role)
    {
        if (prec == Prec.Top || prec == Prec.ArrowRhs) return true;
        if (prec is not Prec.Operand(var outer, var outerRole)) return false;

        RoleException NoOrder() =>
            new($"`{outer}` and `{symbol}` have no declared order; parenthesise one of them");

        switch (outerRole.Order, role.Order)
        {
            case ({ } o, { } i):
                return Order.Relation(i, o) switch
                {
                    OrderRelation.Stronger => true,
                    OrderRelation.Weaker => false,
                    OrderRelation.Same => o.Assoc switch
                    {
                        Assoc.Right => true,
                        Assoc.Left => false,
                        _ => throw new RoleException(
                            $"`{outer}` and `{symbol}` do not chain: their group {o.Name} is assoc(none); parenthesise one of them"),
                    },
                    _ => throw NoOrder(),
                };
            case ({ }, null): return false;
            case (null, { }): return true;
            default: throw NoOrder();
        }
    }

    // ---- uses ---------------------------------------------------------------

    /// <summary>
    /// A token with a prefix role at the head of an expression: a syntax form's
    /// use, or a fixity-only prefix call. Null when the token has no prefix role.
    /// </summary>
    private (Syntax, Terms)? PrefixRoleUse(TokenTree term, Terms rest)
    {
        if (TokenText(term) is not string name || term is not TokenTree.Leaf leaf) return null;
        if (_env.Roles.FindRole(name, Fixity.Prefix, leaf.Token.Scope) is not { } role) return null;

        var id = new Id(name, term.Span, leaf.Token.Scope);
        switch (role.Meaning)
        {
            case RoleMeaning.Rules rules:
            {
                var (inst, after) = InstantiateForm(id, rules.Kind, FormKind.Expr, role.FromUnit, rules.Items,
                    new Terms([term, .. rest]), new Prec.Operand(name, role));
                return (new Syntax.Instantiate(inst, term.Span), after);
            }
            case RoleMeaning.ApplyValue:
            {
                var (rhs, after) = ParseExprPrec(rest, new Prec.Operand(name, role));
                return (new Syntax.Ap(new Syntax.Var(id), Explicitness.Explicit, rhs, SourceSpan.Between(term.Span, rhs.Span)), after);
            }
            case RoleMeaning.CallMacro:
            {
                var (rhs, after) = ParseExprPrec(rest, new Prec.Operand(name, role));
                return (new Syntax.OperatorUse(id, Fixity.Prefix, [rhs], role.DeclaredAt, role.FromUnit, SourceSpan.Between(term.Span, rhs.Span)), after);
            }
            default:
                throw new ExpandException($"not a prefix form: {name}");
        }
    }

    /// <summary>
    /// A token with an infix role after <paramref name="lhs"/>. Null when the
    /// token has no infix role, or its role does not continue at
    /// <paramref name="prec"/>, so the expression ends before it.
    /// </summary>
    private (Syntax, Terms)? InfixRoleUse(Syntax lhs, TokenTree term, Terms rest, Prec prec)
    {
        if (TokenText(term) is not string symbol || term is not TokenTree.Leaf leaf) return null;
        if (_env.Roles.FindRole(symbol, Fixity.Infix, leaf.Token.Scope) is not { } role) return null;
        if (role.Meaning is RoleMeaning.PolyArrow)
            throw new NotImplementedException("not ported yet: the polymorphic arrow `~>`");
        if (!Continues(prec, symbol, role)) return null;

        var (rhs, after) = ParseExprPrec(rest, new Prec.Operand(symbol, role));
        var span = SourceSpan.Between(lhs.Span, rhs.Span);
        var id = new Id(symbol, term.Span, leaf.Token.Scope);
        Syntax use = role.Meaning switch
        {
            RoleMeaning.ApplyValue => new Syntax.Ap(
                new Syntax.Ap(new Syntax.Var(id), Explicitness.Explicit, lhs, span), Explicitness.Explicit, rhs, span),
            RoleMeaning.Rules { Items: [var rule] } => PatternHoles(rule.Pattern) switch
            {
                [var l, var r] => new Syntax.Instantiate(
                    new Instantiation(id, rule, [(l, new Capture.Expr(lhs)), (r, new Capture.Expr(rhs))], role.FromUnit), span),
                _ => throw new ExpandException($"an infix syntax form takes two operands: {symbol}"),
            },
            RoleMeaning.Rules => throw new ExpandException($"an infix syntax form has one rule: {symbol}"),
            RoleMeaning.AssignRef => new Syntax.RefSet(lhs, rhs, span),
            RoleMeaning.CallMacro => new Syntax.OperatorUse(id, Fixity.Infix, [lhs, rhs], role.DeclaredAt, role.FromUnit, span),
            _ => throw new ExpandException($"not an infix operator: {symbol}"),
        };
        return (use, after);
    }

    /// <summary>
    /// A statement that uses a <c>: Decl</c> syntax form: the use, and the
    /// statements after it. Null when the statement's head names no declaration form.
    /// </summary>
    public (Instantiation, Terms)? BlockDeclForm(Terms terms)
    {
        var (stmt, rest) = TakeStatement(terms);
        if (rest.IsEmpty) return null;
        return DeclFormUse(stmt) is { } inst ? (inst, rest) : null;
    }

    private Instantiation? DeclFormUse(Terms stmt)
    {
        stmt = DropSeparators(stmt);
        if (stmt.Head is not TokenTree.Leaf { Token: { Kind: TokenKind.Ident head } token } headTerm) return null;
        if (_env.Roles.FindRole(head.Name, Fixity.Prefix, token.Scope) is not { Meaning: RoleMeaning.Rules { Kind: FormKind.Decl } rules } role)
            return null;
        var (inst, rest) = InstantiateForm(new Id(head.Name, headTerm.Span, token.Scope), rules.Kind, FormKind.Decl,
            role.FromUnit, rules.Items, stmt, new Prec.Operand(head.Name, role));
        EnsureNoRest("declaration syntax template use", rest);
        return inst;
    }

    // ---- declarations -------------------------------------------------------

    /// <summary>
    /// <c>syntax name [: Decl] [group] { rules }</c>, <c>infix (op) [group]
    /// [($a, $b) { body }]</c>, <c>prefix (op) [group]</c> or
    /// <c>order name [: clauses]</c>: the binder it declares and its role. Null
    /// when the statement declares none. An <c>infix</c> whose body is not a template is
    /// a procedural operator macro: its role calls the macro <paramref name="Macro"/>
    /// declares under the same name.
    /// </summary>
    private (Id Name, Role Role, Syntax? Macro)? ParseRoleDecl(Terms stmt) =>
        ParseOperatorMacroDecl(stmt) ?? (ParseRoleOnlyDecl(stmt) is var (name, role) ? (name, role, null) : null);

    /// <summary>
    /// <c>infix (op) [group] (params) { body }</c>: a role that calls the macro of its
    /// name, and that macro, a function of its parameters. Null for any other statement.
    /// </summary>
    private (Id, Role, Syntax?)? ParseOperatorMacroDecl(Terms stmt)
    {
        stmt = DropSeparators(stmt);
        if (stmt.Head is not TokenTree.Leaf { Token.Kind: TokenKind.Ident { Name: "infix" } } infix
            || stmt.Drop(1).Head is not TokenTree.Group { Delimiter: Delimiter.Paren } symbolGroup) return null;
        var (order, value) = ParseJoinedOrder(stmt.Drop(2));
        if (DropSeparators(value).IsEmpty || IsOperatorTemplate(value)) return null;

        var name = OperatorSymbol("infix", symbolGroup);
        value = DropSeparators(value);
        var parameters = value.Head is TokenTree.Group { Delimiter: Delimiter.Paren } group
            ? ParseParamGroup(new Terms(group.Items), Explicitness.Explicit)
            : [];
        var (body, rest, bodySpan) = ParseBody(value.Head is TokenTree.Group { Delimiter: Delimiter.Paren } ? value.Tail : value);
        EnsureNoRest("operator macro", rest);
        var span = SourceSpan.Between(infix.Span, bodySpan);
        var macro = parameters.Reverse().Aggregate(body, (acc, p) => new Syntax.Lam(p, acc, span));
        var (declared, role) = DeclareRole(name, new Role(Fixity.Infix, order, RoleMeaning.CallMacro.Instance, name.Span, null));
        return (declared, role, macro);
    }

    private (Id Name, Role Role)? ParseRoleOnlyDecl(Terms stmt)
    {
        stmt = DropSeparators(stmt);
        if (stmt.Head is not TokenTree.Leaf { Token.Kind: TokenKind.Ident { Name: var keyword } }) return null;

        switch (keyword)
        {
            case "infix" or "prefix" when stmt.Drop(1).Head is TokenTree.Group { Delimiter: Delimiter.Paren } symbolGroup:
            {
                var fixity = keyword == "infix" ? Fixity.Infix : Fixity.Prefix;
                var name = OperatorSymbol(keyword, symbolGroup);
                var (order, value) = ParseJoinedOrder(stmt.Drop(2));
                if (DropSeparators(value).IsEmpty)
                    return DeclareRole(name, new Role(fixity, order, RoleMeaning.ApplyValue.Instance, name.Span, null));
                if (fixity == Fixity.Prefix) throw new ExpandException("prefix operator with a body is not supported");
                // A body that is not a template was read by ParseOperatorMacroDecl.
                return DeclareRole(name, ParseOperatorTemplate(name, order, value));
            }

            case "order" when NameOf(stmt.Drop(1).Head) is Id groupName:
            {
                var after = DropSeparators(stmt.Drop(2));
                if (!after.IsEmpty && !IsToken(after.Head, TokenKind.Colon))
                    throw new ExpandException("an order group is declared order name or order name : clauses");
                return DeclareRole(groupName, ParseOrderDecl(groupName, after.IsEmpty ? after : after.Tail));
            }

            case "syntax" when stmt.Count > 1:
            {
                if (stmt[1] is not TokenTree.Leaf { Token: { Kind: TokenKind.Ident head } token })
                    throw new ExpandException("syntax declaration head must be an identifier");
                var name = new Id(head.Name, stmt[1].Span, token.Scope);
                var after = DropSeparators(stmt.Drop(2));
                var kind = FormKind.Expr;
                if (IsToken(after.Head, TokenKind.Colon) && after.Drop(1).Head is TokenTree.Leaf { Token.Kind: TokenKind.Ident { Name: "Decl" } })
                    (kind, after) = (FormKind.Decl, after.Drop(2));
                var (order, rest) = ParseJoinedOrder(after);
                rest = DropSeparators(rest);
                if (rest.Head is not TokenTree.Group { Delimiter: Delimiter.Brace } body)
                    throw new ExpandException("unsupported syntax declaration shape");
                EnsureNoRest("syntax declaration", rest.Tail);
                var rules = ParseRules(head.Name, kind, new Terms(body.Items));
                CheckTokenHoles(kind, rules);
                return DeclareRole(name, new Role(Fixity.Prefix, order, new RoleMeaning.Rules(kind, rules), name.Span, null));
            }

            default:
                return null;
        }
    }

    /// <summary>Reading quoted syntax, a declared role is registered as it is read; otherwise expansion registers it (M7).</summary>
    private (Id, Role) DeclareRole(Id name, Role role)
    {
        if (_env.Registers)
        {
            _env.Roles.Extend(name.Name, name.Scope, name.Name, BinderMeaning.Role, role);
            _env.Declared++;
        }
        return (name, role);
    }

    /// <summary>The declared operator's name: an id spanning its parenthesised symbol.</summary>
    private Id OperatorSymbol(string keyword, TokenTree.Group group)
    {
        var items = DropSeparators(new Terms(group.Items));
        if (items.Count == 1 && TokenText(items[0]) is "=>")
            throw new ExpandException("=> is reserved and cannot be declared as an operator");
        if (items.Count != 1 || TokenText(items[0]) is not string symbol || items[0] is not TokenTree.Leaf leaf)
            throw new ExpandException($"{keyword} requires a symbol in parens");
        return new Id(symbol, group.Span, leaf.Token.Scope);
    }

    /// <summary>The group an operator or form joins, written after its name: null when it joins none.</summary>
    private (Order?, Terms) ParseJoinedOrder(Terms terms)
    {
        terms = DropSeparators(terms);
        if (terms.Head is TokenTree.Leaf { Token.Kind: TokenKind.Int })
            throw new ExpandException("numeric precedence was removed; declare an order group (order g : stronger_than(…)) and write infix (op) g");
        if (terms.Head is not TokenTree.Leaf { Token.Kind: TokenKind.Ident })
            return (null, terms);
        var (reference, rest) = TakeOrderRef(terms);
        return (ResolveOrder(reference), rest);
    }

    /// <summary>The terms naming one group at the front -- <c>g</c> or <c>M.N.g</c> -- and the terms after them.</summary>
    private (Terms, Terms) TakeOrderRef(Terms terms)
    {
        var n = 1;
        while (n + 1 < terms.Count && IsToken(terms[n], TokenKind.Dot) && terms[n + 1] is TokenTree.Leaf { Token.Kind: TokenKind.Ident })
            n += 2;
        return (TakeTerms(terms, n), terms.Drop(n));
    }

    /// <summary>An order group named where a declaration names it: a bare name resolves by scope set, like any binder.</summary>
    private Order ResolveOrder(Terms reference)
    {
        var leaf = (TokenTree.Leaf)reference[0];
        var name = ((TokenKind.Ident)leaf.Token.Kind).Name;
        if (reference.Count == 1)
            return _env.Roles.FindOrder(name, leaf.Token.Scope)
                ?? throw new ExpandException($"unknown order group: {name}");

        // `M.g`: `g` among the roles the unit `M` denotes exports.
        if (reference.Count != 3)
            throw new NotImplementedException("not ported yet: an order group named through a unit member's path");
        var group = ((TokenKind.Ident)((TokenTree.Leaf)reference[2]).Token.Kind).Name;
        var unit = new Syntax.Var(new Id(name, leaf.Span, leaf.Token.Scope));
        return (_env.UnitRoles(unit) ?? []).FirstOrDefault(r => r.Name == group && r.Role.Meaning is RoleMeaning.OrderGroup).Role?.Order
            ?? throw new ExpandException($"unknown order group: {name}.{group}");
    }

    /// <summary>
    /// <c>order name : stronger_than(g, …) weaker_than(g, …) weakest assoc(left|right|none)</c>:
    /// precedence is relative, and a declaration that would make the order cyclic is an error.
    /// </summary>
    private Role ParseOrderDecl(Id name, Terms clauses)
    {
        var stronger = new List<Order>();
        var weaker = new List<Order>();
        var assoc = Assoc.Left;
        var weakest = false;

        List<Order> Groups(TokenTree.Group group) => SplitCommas(new Terms(group.Items)).Select(ts =>
        {
            ts = DropSeparators(ts);
            var (reference, rest) = TakeOrderRef(ts);
            if (ts.IsEmpty || ts.Head is not TokenTree.Leaf { Token.Kind: TokenKind.Ident } || !DropSeparators(rest).IsEmpty)
                throw new ExpandException("expected an order group");
            return ResolveOrder(reference);
        }).ToList();

        clauses = DropSeparators(clauses);
        while (!clauses.IsEmpty)
        {
            switch (clauses.Head, clauses.Drop(1).Head)
            {
                case (TokenTree.Leaf { Token.Kind: TokenKind.Ident { Name: "weakest" } }, _):
                    weakest = true;
                    clauses = clauses.Drop(1);
                    break;
                case (TokenTree.Leaf { Token.Kind: TokenKind.Ident { Name: "stronger_than" } }, TokenTree.Group { Delimiter: Delimiter.Paren } g):
                    stronger.AddRange(Groups(g));
                    clauses = clauses.Drop(2);
                    break;
                case (TokenTree.Leaf { Token.Kind: TokenKind.Ident { Name: "weaker_than" } }, TokenTree.Group { Delimiter: Delimiter.Paren } g):
                    weaker.AddRange(Groups(g));
                    clauses = clauses.Drop(2);
                    break;
                case (TokenTree.Leaf { Token.Kind: TokenKind.Ident { Name: "assoc" } }, TokenTree.Group { Delimiter: Delimiter.Paren } g):
                    assoc = DropSeparators(new Terms(g.Items)) switch
                    {
                        [TokenTree.Leaf { Token.Kind: TokenKind.Ident { Name: "left" } }] => Assoc.Left,
                        [TokenTree.Leaf { Token.Kind: TokenKind.Ident { Name: "right" } }] => Assoc.Right,
                        [TokenTree.Leaf { Token.Kind: TokenKind.Ident { Name: "none" } }] => Assoc.None,
                        _ => throw new ExpandException("assoc is written assoc(left), assoc(right) or assoc(none)"),
                    };
                    clauses = clauses.Drop(2);
                    break;
                default:
                    throw new ExpandException("an order clause is stronger_than(…), weaker_than(…), weakest or assoc(left|right|none)");
            }
            clauses = DropSeparators(clauses);
        }

        foreach (var st in stronger)
            foreach (var wk in weaker)
                if (Order.Relation(st, wk) is OrderRelation.Same or OrderRelation.Stronger)
                    throw new ExpandException(
                        $"order {name.Name} would be both stronger than {st.Name} and weaker than {wk.Name}: the order would be cyclic");

        var order = new Order($"{name.Name}@{Interlocked.Increment(ref _orderCounter)}", name.Name, assoc, weakest, [.. stronger], [.. weaker]);
        return new Role(Fixity.Prefix, order, RoleMeaning.OrderGroup.Instance, name.Span, null);
    }

    /// <summary><c>($a, $b) { body }</c> after an infix declaration: a template, not a procedural macro.</summary>
    private bool IsOperatorTemplate(Terms value) =>
        DropSeparators(value).Head is TokenTree.Group { Delimiter: Delimiter.Paren } group
        && DropSeparators(new Terms(group.Items)).Head is TokenTree.Leaf { Token.Kind: TokenKind.Operator { Spelling: "$" } };

    /// <summary><c>infix (op) g ($lhs, $rhs) { body }</c>: one rule, <c>$lhs op $rhs</c>, whose replacement is the brace group.</summary>
    private Role ParseOperatorTemplate(Id symbol, Order? order, Terms value)
    {
        value = DropSeparators(value);
        var paramsGroup = (TokenTree.Group)value.Head!;
        var holes = SplitCommas(new Terms(paramsGroup.Items)).Select(ts => DropSeparators(ts) switch
        {
            [TokenTree.Leaf { Token.Kind: TokenKind.Operator { Spelling: "$" } }, TokenTree.Leaf { Token.Kind: TokenKind.Ident hole }] => hole.Name,
            _ => throw new ExpandException("operator template params must be $hole names"),
        }).ToList();
        if (DropSeparators(value.Tail) is not [TokenTree.Group { Delimiter: Delimiter.Brace } body])
            throw new ExpandException("expected { body } after operator template parameters");
        if (holes.Count != 2) throw new ExpandException("operator template must have 2 holes");

        var replacementTerms = RewriteHoles(new Terms([body]));
        CheckReplacementHoles(holes, replacementTerms);
        var literal = new TokenTree.Leaf(new Token(new TokenKind.Operator(symbol.Name), symbol.Span));
        var rule = new Rule(
            [new RulePart.Hole(holes[0], HoleKind.Expr, symbol.Span), new RulePart.Literal(literal), new RulePart.Hole(holes[1], HoleKind.Expr, symbol.Span)],
            ParseReplacement(FormKind.Expr, holes, replacementTerms),
            SourceSpan.Between(symbol.Span, body.Span));
        return new Role(Fixity.Infix, order, new RoleMeaning.Rules(FormKind.Expr, [rule]), symbol.Span, null);
    }

    // ---- rules --------------------------------------------------------------

    private EquatableArray<Rule> ParseRules(string head, FormKind kind, Terms body)
    {
        var available = _env.Holes;
        var rules = new List<Rule>();
        foreach (var ruleTerms in SplitMatchBranches(body))
        {
            var arrow = IndexOf(ruleTerms, IsFatArrow);
            if (arrow < 0) throw new ExpandException("syntax declaration rule requires => between pattern and replacement");
            var pattern = ParseRulePattern(RewriteHoles(Slice(ruleTerms, 0, arrow)));
            if (pattern.FirstOrDefault() is not RulePart.Literal { Term: var first } || Spelling(first) != head)
                throw new ExpandException($"syntax branch pattern must start with declared head: {head}");
            var replacement = RewriteHoles(ruleTerms.Drop(arrow + 1));
            var holes = PatternHoles(pattern);
            CheckReplacementHoles([.. holes, .. available], replacement);
            rules.Add(new Rule(pattern, ParseReplacement(kind, [.. holes, .. available], replacement), ruleTerms.Span));
        }
        return [.. rules];
    }

    /// <summary>A replacement, read as quoted syntax with <paramref name="holes"/> the captures it may use.</summary>
    private Replacement ParseReplacement(FormKind kind, IEnumerable<string> holes, Terms terms)
    {
        var quoted = new Enforest(_env.Quoted(holes));
        if (kind == FormKind.Expr) return new Replacement.Expr(quoted.ParseAll(terms));
        if (DropSeparators(terms) is not [TokenTree.Group { Delimiter: Delimiter.Brace } body])
            throw new ExpandException("a Decl syntax form's replacement is written { declarations }");
        return new Replacement.Decls(quoted.ReadItemsNow(new Terms(body.Items)));
    }

    /// <summary>
    /// <c>$x</c> as two tokens, rewritten to one identifier spelled <c>$x</c> --
    /// <c>$</c> cannot begin a source identifier -- so quoted syntax parses as written.
    /// </summary>
    private Terms RewriteHoles(Terms terms)
    {
        var output = new List<TokenTree>();
        for (var i = 0; i < terms.Count; i++)
        {
            var term = terms[i];
            if (term is TokenTree.Leaf { Token.Kind: TokenKind.Operator { Spelling: "$" } }
                && i + 1 < terms.Count && terms[i + 1] is TokenTree.Leaf { Token: { Kind: TokenKind.Ident name } token } next)
            {
                output.Add(new TokenTree.Leaf(token with { Kind = new TokenKind.Ident("$" + name.Name) }));
                i++;
                continue;
            }
            output.Add(term is TokenTree.Group g ? g with { Items = RewriteHoles(new Terms(g.Items)).ToArray() } : term);
        }
        return new Terms([.. output]);
    }

    /// <summary>The hole an identifier spelled <c>$x</c> names.</summary>
    private string? HoleName(TokenTree term) =>
        term is TokenTree.Leaf { Token.Kind: TokenKind.Ident { Name: ['$', _, ..] name } } ? name[1..] : null;

    private EquatableArray<RulePart> ParseRulePattern(Terms terms)
    {
        terms = DropSeparators(terms);
        // The head is a literal: a hole written there, in a rule a replacement
        // declares, names the head with an enclosing capture (M7 decision 6).
        if (terms.Head is { } head && HoleName(head) is not null)
            return [new RulePart.Literal(head), .. ParsePatternParts(terms.Tail)];
        return ParsePatternParts(terms);
    }

    private EquatableArray<RulePart> ParsePatternParts(Terms terms)
    {
        var parts = new List<RulePart>();
        terms = DropSeparators(terms);
        while (!terms.IsEmpty)
        {
            var term = terms[0];
            if (term is TokenTree.Leaf { Token.Kind: TokenKind.Operator { Spelling: "$" } }
                && terms.Drop(1).Head is TokenTree.Group { Delimiter: Delimiter.Paren } annotated)
            {
                parts.Add(ParseAnnotatedHole(annotated));
                terms = terms.Drop(2);
                continue;
            }
            if (HoleName(term) is string hole)
                parts.Add(new RulePart.Hole(hole, HoleKind.Expr, term.Span));
            else if (term is TokenTree.Group g)
                parts.Add(new RulePart.Group(g.Delimiter, ParsePatternParts(new Terms(g.Items)), g.Span));
            else
                parts.Add(new RulePart.Literal(term));
            terms = terms.Tail;
        }
        return [.. parts];
    }

    /// <summary><c>$(name : Kind)</c>, kinds written as their reflection types.</summary>
    private RulePart.Hole ParseAnnotatedHole(TokenTree.Group group)
    {
        var items = DropSeparators(new Terms(group.Items));
        if (items.Count >= 3 && items[0] is TokenTree.Leaf { Token.Kind: TokenKind.Ident name } && IsToken(items[1], TokenKind.Colon))
        {
            switch (items.Drop(2))
            {
                case [TokenTree.Leaf { Token.Kind: TokenKind.Ident kind }]:
                    return new RulePart.Hole(name.Name, kind.Name switch
                    {
                        "Expr" => HoleKind.Expr,
                        "Block" => HoleKind.Block,
                        "Id" => HoleKind.Id,
                        "Decl" => HoleKind.Decl,
                        "Pattern" => HoleKind.Pattern,
                        "expr" or "block" or "binder" or "ident" or "decl" =>
                            throw new ExpandException($"hole kinds are written as types (Expr, Block, Id, Decl, Pattern), not {kind.Name}"),
                        _ => throw new ExpandException($"unknown syntax template hole kind: {kind.Name}"),
                    }, group.Span);
                case [TokenTree.Leaf { Token.Kind: TokenKind.Ident { Name: "List" } }, TokenTree.Group { Delimiter: Delimiter.Paren } arg]:
                    return new RulePart.Hole(name.Name, DropSeparators(new Terms(arg.Items)) switch
                    {
                        [TokenTree.Leaf { Token.Kind: TokenKind.Ident { Name: "Decl" } }] => HoleKind.Decls,
                        [TokenTree.Leaf { Token.Kind: TokenKind.Ident { Name: "TokenTree" } }] => HoleKind.Tokens,
                        _ => throw new ExpandException("a list hole is $(name : List(Decl)) or $(name : List(TokenTree))"),
                    }, group.Span);
            }
        }
        throw new ExpandException("expected template hole annotation $(name : Kind)");
    }

    private EquatableArray<string> PatternHoles(EquatableArray<RulePart> parts)
    {
        var holes = new List<string>();
        void Go(IEnumerable<RulePart> ps)
        {
            foreach (var p in ps)
                switch (p)
                {
                    case RulePart.Hole h:
                        if (holes.Contains(h.Name)) throw new ExpandException($"duplicate syntax pattern hole: {h.Name}");
                        holes.Add(h.Name);
                        break;
                    case RulePart.Group g:
                        Go(g.Parts);
                        break;
                }
        }
        Go(parts);
        return [.. holes];
    }

    /// <summary>
    /// Every hole a replacement uses must be a capture of its rule or of an
    /// enclosing one; a rule the replacement itself declares binds its own.
    /// </summary>
    private void CheckReplacementHoles(IEnumerable<string> bound, Terms replacement)
    {
        var boundSet = bound.ToHashSet();
        foreach (var hole in ReplacementHoles(boundSet, replacement))
            throw new ExpandException($"unbound syntax template hole in replacement: {hole}");
    }

    private IEnumerable<string> ReplacementHoles(HashSet<string> bound, Terms terms)
    {
        for (var i = 0; i < terms.Count; i++)
        {
            var term = terms[i];
            if (HoleName(term) is string hole)
            {
                if (!bound.Contains(hole)) yield return hole;
            }
            else if (term is TokenTree.Leaf { Token.Kind: TokenKind.Ident { Name: "syntax" } } && i + 1 < terms.Count)
            {
                // A nested declaration: its head may be an enclosing hole; its
                // rules bind their own.
                if (HoleName(terms[i + 1]) is string head && !bound.Contains(head)) yield return head;
                var j = i + 2;
                while (j < terms.Count && terms[j] is not TokenTree.Group { Delimiter: Delimiter.Brace }) j++;
                if (j >= terms.Count) continue;
                foreach (var ruleTerms in SplitMatchBranches(new Terms(((TokenTree.Group)terms[j]).Items)))
                {
                    var arrow = IndexOf(ruleTerms, IsFatArrow);
                    if (arrow < 0) continue;
                    var pattern = ParseRulePattern(Slice(ruleTerms, 0, arrow));
                    if (pattern.FirstOrDefault() is RulePart.Literal { Term: var h } && HoleName(h) is string ruleHead && !bound.Contains(ruleHead))
                        yield return ruleHead;
                    foreach (var inner in ReplacementHoles([.. bound, .. PatternHoles(pattern)], ruleTerms.Drop(arrow + 1)))
                        yield return inner;
                }
                i = j;
            }
            else if (term is TokenTree.Group g)
            {
                foreach (var inner in ReplacementHoles(bound, new Terms(g.Items))) yield return inner;
            }
        }
    }

    /// <summary>A <c>List(TokenTree)</c> hole takes the rest of a declaration use: its rule's last part, outside any group.</summary>
    private void CheckTokenHoles(FormKind kind, EquatableArray<Rule> rules)
    {
        static string? Inside(RulePart p) => p switch
        {
            RulePart.Hole { Kind: HoleKind.Tokens } h => h.Name,
            RulePart.Group g => g.Parts.Select(Inside).FirstOrDefault(n => n is not null),
            _ => null,
        };
        foreach (var rule in rules)
        {
            var parts = rule.Pattern;
            var beforeLast = parts.Take(Math.Max(0, parts.Length - 1));
            var lastGroup = parts.Length > 0 && parts[^1] is RulePart.Group g ? [g] : Array.Empty<RulePart>();
            if (beforeLast.Concat(lastGroup).Select(Inside).FirstOrDefault(n => n is not null) is string hole)
                throw new ExpandException($"the List(TokenTree) hole {hole} takes the rest of the use: it must be the rule's last part");
            if (kind != FormKind.Decl && parts.Any(p => Inside(p) is not null))
                throw new ExpandException("a List(TokenTree) hole takes the rest of a declaration: the form must be : Decl");
        }
    }

    private string? Spelling(TokenTree term) => term switch
    {
        TokenTree.Leaf { Token.Kind: TokenKind.Ident i } => i.Name,
        TokenTree.Leaf { Token.Kind: TokenKind.Operator o } => o.Spelling,
        TokenTree.Leaf { Token.Kind: TokenKind.Word w } => w.Spelling,
        _ => null,
    };

    private bool SameLiteral(TokenTree expected, TokenTree actual) => (expected, actual) switch
    {
        (TokenTree.Leaf { Token.Kind: TokenKind.Int a }, TokenTree.Leaf { Token.Kind: TokenKind.Int b }) => a.Value == b.Value,
        (TokenTree.Leaf { Token.Kind: TokenKind.Char a }, TokenTree.Leaf { Token.Kind: TokenKind.Char b }) => a.Value == b.Value,
        (TokenTree.Leaf { Token.Kind: TokenKind.Str a }, TokenTree.Leaf { Token.Kind: TokenKind.Str b }) => a.Value == b.Value,
        (TokenTree.Leaf, TokenTree.Leaf) => Spelling(expected) is { } s && s == Spelling(actual),
        _ => false,
    };

    // ---- instantiation ------------------------------------------------------

    /// <summary>A syntax form's use: the first rule whose pattern matches, and what its holes captured.</summary>
    private (Instantiation, Terms) InstantiateForm(
        Id form, FormKind kind, FormKind position, string? fromUnit, EquatableArray<Rule> rules, Terms terms, Prec trailing)
    {
        // M8: a syntax form is used only where its kind's position is.
        if (kind != position)
            throw new ExpandException($"syntax form '{form.Name}' has kind {kind} but was used in {position} context");
        foreach (var rule in rules)
            if (MatchParts(rule.Pattern, 0, terms, [], whole: false, trailing) is var (captures, rest))
                return (new Instantiation(form, rule, [.. captures], fromUnit), rest);
        throw new ExpandException($"no matching branch for syntax {form.Name}");
    }

    /// <summary>
    /// How far a hole reads is structural: the hole ending a use reads its form's
    /// operand; one followed by <c>,</c> or <c>;</c> reads to it; any other hole
    /// is exactly one term.
    /// </summary>
    private enum Extent { Trailing, ToSeparator, OneTerm }

    private Extent ExtentAfter(EquatableArray<RulePart> parts, int next) =>
        next >= parts.Length ? Extent.Trailing
        : parts[next] is RulePart.Literal { Term: var t } && (IsToken(t, TokenKind.Comma) || IsSeparator(t)) ? Extent.ToSeparator
        : Extent.OneTerm;

    private (List<(string, Capture)>, Terms)? MatchParts(
        EquatableArray<RulePart> parts, int index, Terms input, List<(string, Capture)> captures, bool whole, Prec trailing)
    {
        if (index >= parts.Length)
            return whole && !DropSeparators(input).IsEmpty ? null : (captures, input);

        var next = index + 1;
        (List<(string, Capture)>, Terms)? Continue(Terms rest, (string, Capture)? capture = null) =>
            MatchParts(parts, next, rest, capture is { } c ? [.. captures, c] : captures, whole, trailing);

        input = DropSeparators(input);
        switch (parts[index])
        {
            case RulePart.Literal literal:
                return input.Head is { } actualTerm && SameLiteral(literal.Term, actualTerm) ? Continue(input.Tail) : null;

            case RulePart.Group group:
            {
                if (input.Head is not TokenTree.Group actualGroup || actualGroup.Delimiter != group.Delimiter) return null;
                return MatchParts(group.Parts, 0, new Terms(actualGroup.Items), captures, whole: true, Prec.Top) is var (inner, _)
                    ? MatchParts(parts, next, input.Tail, inner, whole, trailing)
                    : null;
            }

            case RulePart.Hole hole:
            {
                if (input.IsEmpty) return null;
                var extent = ExtentAfter(parts, next);
                switch (hole.Kind)
                {
                    case HoleKind.Id:
                        return input.Head is TokenTree.Leaf { Token: { Kind: TokenKind.Ident or TokenKind.Operator } token }
                            ? Continue(input.Tail, (hole.Name, new Capture.Id(token)))
                            : null;

                    case HoleKind.Block:
                        if (input.Head is not TokenTree.Group { Delimiter: Delimiter.Brace } block) return null;
                        return Continue(input.Tail, (hole.Name, _env.Eager
                            ? new Capture.Expr(ParseAll(new Terms([block])))
                            : new Capture.Block(block.Items)));

                    case HoleKind.Expr:
                    {
                        var (expr, rest) = extent switch
                        {
                            Extent.Trailing => ParseExprPrec(input, trailing),
                            Extent.ToSeparator => ParseExprPrec(input, Prec.Top),
                            _ => (ParseAll(new Terms([input[0]])), input.Tail),
                        };
                        return Continue(rest, (hole.Name, new Capture.Expr(expr)));
                    }

                    case HoleKind.Pattern:
                    {
                        var length = extent switch
                        {
                            Extent.Trailing => input.Count,
                            Extent.ToSeparator => IndexOf(input, t => IsToken(t, TokenKind.Comma) || IsSeparator(t)) is var s and >= 0 ? s : input.Count,
                            _ => 1,
                        };
                        return Continue(input.Drop(length), (hole.Name, new Capture.Pattern(ParsePattern(TakeTerms(input, length)))));
                    }

                    case HoleKind.Tokens:
                        return next >= parts.Length ? Continue(Terms.Empty, (hole.Name, new Capture.Tokens(input.ToArray()))) : null;

                    default:
                    {
                        // Declarations, captured unread: to the separator, the rest
                        // of the use, or the items of one brace group.
                        (Terms Decls, Terms After)? taken = extent switch
                        {
                            Extent.Trailing => (input, Terms.Empty),
                            Extent.ToSeparator => IndexOf(input, t => IsToken(t, TokenKind.Comma) || IsSeparator(t)) is var s and >= 0
                                ? (TakeTerms(input, s), input.Drop(s))
                                : (input, Terms.Empty),
                            _ => input.Head is TokenTree.Group { Delimiter: Delimiter.Brace } g ? (new Terms(g.Items), input.Tail) : null,
                        };
                        if (taken is not var (decls, after)) return null;
                        decls = DropSeparators(decls);
                        if (decls is [TokenTree.Group { Delimiter: Delimiter.Brace } braced]) decls = DropSeparators(new Terms(braced.Items));
                        if (decls.IsEmpty) return null;
                        if (hole.Kind == HoleKind.Decl)
                            return Statements(decls).Count() == 1
                                ? Continue(after, (hole.Name, new Capture.Decl(new Binding.Items(decls.ToArray()))))
                                : null;
                        return Continue(after, (hole.Name, new Capture.Decls([new Binding.Items(decls.ToArray())])));
                    }
                }
            }

            default:
                throw new InvalidOperationException($"unhandled rule part {parts[index].GetType().Name}");
        }
    }

    // ---- reading quoted syntax now ------------------------------------------

    /// <summary>
    /// A definition context's statements, read in order with <paramref name="read"/>.
    /// A statement that declared a role adds a fresh scope to the statements after
    /// it, so the role is visible after it and neither before it nor outside.
    /// </summary>
    private List<T> ReadContext<T>(Terms terms, Func<Terms, bool, T> read)
    {
        var results = new List<T>();
        while (true)
        {
            var (stmt, after) = TakeStatement(terms);
            var rest = DropSeparators(after);
            var declared = _env.Declared;
            if (!stmt.IsEmpty) results.Add(read(stmt, rest.IsEmpty));
            if (rest.IsEmpty) return results;
            if (_env.Declared != declared)
                rest = new Terms([.. rest.Select(t => t.AddScope(FreshQuotedScope()))]);
            terms = rest;
        }
    }

    /// <summary>Module items read now, as quoted syntax is.</summary>
    private EquatableArray<Binding> ReadItemsNow(Terms items) =>
        [.. ReadContext(items, (stmt, _) => ParseModuleStatement(stmt)).SelectMany(b => b)];

    /// <summary>A <c>{ … }</c> body: left unread for expansion, or read now when reading quoted syntax.</summary>
    private Syntax ReadBlock(EquatableArray<TokenTree> items, SourceSpan span)
    {
        if (!_env.Eager) return new Syntax.Block(items, span);

        var terms = new Terms(items);
        var discards = items.Length > 0 && IsSeparator(items[^1]);
        var statements = ReadContext(terms, (stmt, last) => last && !discards
            ? (Value: ParseAll(stmt), Wrapper: (Syntax?)null)
            : (Value: (Syntax?)null, Wrapper: DoStatement(span, stmt, EagerBodyPlaceholder)));
        if (statements.Count == 0) throw new ExpandException("empty block");

        var body = statements[^1].Value ?? Unit(span);
        for (var i = statements.Count - 1; i >= 0; i--)
            if (statements[i].Wrapper is { } wrapper) body = WithBody(wrapper, body);
        return body;
    }

    private static readonly Syntax EagerBodyPlaceholder = new Syntax.Atom(Atom.Unit.Instance, SourceSpan.Synthetic);

    /// <summary>A statement's form, read before the statements after it, given the body it scopes over.</summary>
    private Syntax WithBody(Syntax statement, Syntax body) => statement switch
    {
        Syntax.Let l => l with { Body = body },
        Syntax.LetRecGroup g => g with { Body = body },
        Syntax.Open o => o with { Body = body },
        Syntax.SyntaxDef d => d with { Body = body },
        Syntax.MacroDef d => d with { Body = body },
        _ => throw new NotImplementedException($"not ported yet: reading the statement {statement.GetType().Name} as quoted syntax"),
    };
}
