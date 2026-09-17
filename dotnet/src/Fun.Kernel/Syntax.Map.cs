namespace Fun.Kernel;

/// <summary>
/// What a structural traversal does at each kind of node: <see cref="Id"/> on
/// every identifier, binder and occurrence alike, and on every unread token's
/// scope set seen as an id; then <see cref="Form"/>, <see cref="Binding"/>,
/// <see cref="Pattern"/> and <see cref="Token"/> bottom-up on what it rebuilds.
/// A syntax form's use and the rules a declaration writes are what filling a
/// template has to treat specially.
/// </summary>
public sealed class SyntaxMapper
{
    public Func<Id, Id> Id { get; init; } = id => id;
    public Func<Token, Token> Token { get; init; } = token => token;
    public Func<Syntax, Syntax> Form { get; init; } = form => form;
    public Func<Binding, Binding> Binding { get; init; } = binding => binding;
    public Func<Pattern, Pattern> Pattern { get; init; } = pattern => pattern;

    /// <summary>A capture replaced whole; null maps its parts.</summary>
    public Func<Capture, Capture?> Capture { get; init; } = _ => null;

    /// <summary>A rule a declaration in the tree writes: its pattern tokens and replacement are mapped.</summary>
    public Func<SyntaxMapper, Rule, Rule> Rule { get; init; } = MapRuleDefault;

    /// <summary>
    /// The rule a use names: data from where its form was declared, left alone
    /// unless this is set.
    /// </summary>
    public Func<SyntaxMapper, Rule, Rule>? UsedRule { get; init; }

    /// <summary>Every identifier through <paramref name="id"/>, nothing else.</summary>
    public static SyntaxMapper OfIds(Func<Id, Id> id) => new() { Id = id };

    /// <summary><paramref name="scope"/> added to every identifier and unread token: how a binder marks its body.</summary>
    public static SyntaxMapper Adding(ScopeSet scope) => OfIds(id => id with { Scope = id.Scope.Union(scope) });

    public static Rule MapRuleDefault(SyntaxMapper m, Rule rule)
    {
        RulePart Part(RulePart part) => part switch
        {
            RulePart.Literal l => l with { Term = l.Term.Map(m) },
            RulePart.Group g => g with { Parts = [.. g.Parts.Select(Part)] },
            _ => part,
        };
        return rule with
        {
            Pattern = [.. rule.Pattern.Select(Part)],
            Replacement = rule.Replacement switch
            {
                Replacement.Expr e => e with { Syntax = e.Syntax.Map(m) },
                Replacement.Decls d => d with { Bindings = [.. d.Bindings.Select(b => b.Map(m))] },
                _ => throw new InvalidOperationException($"unhandled replacement {rule.Replacement.GetType().Name}"),
            },
        };
    }

    /// <summary>The id an identifier or operator token names; its scope set is the token's.</summary>
    public static Id TokenId(Token token) =>
        new(token.Kind switch
        {
            TokenKind.Ident i => i.Name,
            TokenKind.Operator o => o.Spelling,
            _ => "",
        }, token.Span, token.Scope);

    public Token MapToken(Token token) => Token(token with { Scope = Id(TokenId(token)).Scope });

    public Role MapRole(Role role) => role.Meaning is RoleMeaning.Rules rules
        ? role with { Meaning = rules with { Items = [.. rules.Items.Select(r => Rule(this, r))] } }
        : role;

    public Capture MapCapture(Capture capture) => Capture(capture) ?? capture switch
    {
        Fun.Kernel.Capture.Expr e => e with { Syntax = e.Syntax.Map(this) },
        Fun.Kernel.Capture.Block b => b with { Terms = [.. b.Terms.Select(t => t.Map(this))] },
        Fun.Kernel.Capture.Tokens t => t with { Terms = [.. t.Terms.Select(x => x.Map(this))] },
        Fun.Kernel.Capture.Id i => i with { Token = MapToken(i.Token) },
        Fun.Kernel.Capture.Pattern p => p with { Value = p.Value.Map(this) },
        Fun.Kernel.Capture.Decls d => d with { Bindings = [.. d.Bindings.Select(b => b.Map(this))] },
        Fun.Kernel.Capture.Decl d => d with { Binding = d.Binding.Map(this) },
        _ => throw new InvalidOperationException($"unhandled capture {capture.GetType().Name}"),
    };

    public Instantiation MapInstantiation(Instantiation inst) => inst with
    {
        Form = Id(inst.Form),
        Rule = UsedRule is null ? inst.Rule : UsedRule(this, inst.Rule),
        Captures = [.. inst.Captures.Select(c => (c.Hole, MapCapture(c.Capture)))],
    };
}

public abstract partial record Syntax
{
    /// <summary>The one structural traversal, bottom-up: see <see cref="SyntaxMapper"/>.</summary>
    public Syntax Map(SyntaxMapper m)
    {
        Syntax Go(Syntax s) => s.Map(m);
        Syntax? GoOpt(Syntax? s) => s?.Map(m);

        Syntax mapped = this switch
        {
            // Where an import is written is a scope set like an id's.
            Import i => i with { Scope = m.Id(new Id("", i.Span, i.Scope)).Scope },
            Atom or Self or SelfType => this,
            Var v => v with { Id = m.Id(v.Id) },
            Ap a => a with { Fn = Go(a.Fn), Arg = Go(a.Arg) },
            Lam l => l with { Param = MapParam(l.Param, m), Body = Go(l.Body) },
            Let l => l with { Name = m.Id(l.Name), Type = GoOpt(l.Type), Value = Go(l.Value), Body = Go(l.Body) },
            Annotated a => a with { Inner = Go(a.Inner), Type = Go(a.Type) },
            Arrow a => a with
            {
                Name = a.Name is null ? null : m.Id(a.Name),
                Domain = Go(a.Domain),
                Row = a.Row is null ? null : a.Row with
                {
                    Effects = [.. a.Row.Effects.Select(Go)],
                    Tails = [.. a.Row.Tails.Select(Go)],
                },
                Codomain = Go(a.Codomain),
            },
            Prod p => p with { Items = [.. p.Items.Select(Go)] },
            ProdTy p => p with { Items = [.. p.Items.Select(Go)] },
            Proj p => p with { Of = Go(p.Of) },
            FieldAccess f => f with { Of = Go(f.Of) },
            Block b => b with { Terms = [.. b.Terms.Select(t => t.Map(m))] },
            Module mo => mo with { Bindings = [.. mo.Bindings.Select(b => b.Map(m))] },
            Open o => o with { Of = Go(o.Of), Body = Go(o.Body) },
            OpenChoice c => c with { Name = m.Id(c.Name) },
            Match ma => ma with
            {
                Scrutinee = Go(ma.Scrutinee),
                // An effect branch keeps its operation path, mapped like any form.
                Branches = [.. ma.Branches.Select(b => new MatchBranch(b.Pattern.Map(m), Go(b.Body)) { Operation = (FieldAccess?)b.Operation?.Map(m) })],
            },
            Enum e => e with { Constructors = [.. e.Constructors.Select(c => c with { Payloads = [.. c.Payloads.Select(Go)] })] },
            LetRecGroup g => g with { Members = [.. g.Members.Select(x => new RecMember(m.Id(x.Name), Go(x.Value)))], Body = Go(g.Body) },
            Struct st => st with { Bindings = [.. st.Bindings.Select(b => b.Map(m))] },
            Sig sg => sg with { Bindings = [.. sg.Bindings.Select(b => b.Map(m))] },
            RecordConstruct r => r with { Type = Go(r.Type), Fields = [.. r.Fields.Select(f => (f.Name, Go(f.Value)))] },
            SyntaxDef d => d with { Name = m.Id(d.Name), Role = m.MapRole(d.Role), Body = Go(d.Body) },
            Instantiate i => i with { Instantiation = m.MapInstantiation(i.Instantiation) },
            TraitDef t => t with { Name = m.Id(t.Name), Param = m.Id(t.Param), Fields = [.. t.Fields.Select(f => (f.Name, Go(f.Type)))], Body = Go(t.Body) },
            ImplDef i => i with
            {
                Name = i.Name is null ? null : m.Id(i.Name),
                TraitPath = Go(i.TraitPath),
                Arg = Go(i.Arg),
                Fields = [.. i.Fields.Select(f => (f.Name, Go(f.Value)))],
                Body = Go(i.Body),
            },
            TraitBoundSet b => b with { Traits = [.. b.Traits.Select(Go)] },
            PatternSynonym s => s with { Params = [.. s.Params.Select(m.Id)], Rhs = s.Rhs.Map(m) },

            EffectDef d => d with
            {
                Name = m.Id(d.Name),
                Params = [.. d.Params.Select(m.Id)],
                Ops = [.. d.Ops.Select(o => o with { Input = Go(o.Input), Output = Go(o.Output) })],
                Body = Go(d.Body),
            },
            Perform p => p with { Operation = (FieldAccess)Go(p.Operation), Arg = Go(p.Arg) },
            Resume r => r with { Arg = Go(r.Arg) },
            RefNew n => n with { Arg = Go(n.Arg) },
            RefGet g => g with { Ref = Go(g.Ref) },
            RefSet r => r with { Ref = Go(r.Ref), Value = Go(r.Value) },
            MacroDef d => d with { Name = m.Id(d.Name), Value = Go(d.Value), Body = Go(d.Body), Output = GoOpt(d.Output) },
            MacroCall c => c with { Head = Go(c.Head), Args = [.. c.Args.Select(m.MapCapture)] },
            OperatorUse u => u with { Operator = m.Id(u.Operator), Operands = [.. u.Operands.Select(Go)] },
            Quote q => q with { Template = Go(q.Template), Holes = [.. q.Holes.Select(h => (h.Hole, Go(h.Value)))] },
            QuoteDecls q => q with { Items = [.. q.Items.Select(b => b.Map(m))], Holes = [.. q.Holes.Select(h => (h.Hole, Go(h.Value)))] },
            Stx x => x with { Inner = Go(x.Inner) },
            // Written after expansion, where its form was already elaborated: left alone.
            Elaborated => this,
            _ => throw new NotImplementedException($"not ported yet: a syntax traversal over {GetType().Name}"),
        };
        return m.Form(mapped);
    }

    private static Param MapParam(Param p, SyntaxMapper m) =>
        p with { Name = m.Id(p.Name), Type = p.Type?.Map(m) };
}

public abstract partial record Binding
{
    public Binding Map(SyntaxMapper m)
    {
        Binding mapped = this switch
        {
            Let l => l with { Name = m.Id(l.Name), Value = l.Value.Map(m) },
            Open o => o with { Of = o.Of.Map(m) },
            Items i => i with { Terms = [.. i.Terms.Select(t => t.Map(m))] },
            RecGroup g => g with { Members = [.. g.Members.Select(x => new RecMember(m.Id(x.Name), x.Value.Map(m)))] },
            Field f => f with { Type = f.Type.Map(m) },
            Method me => me with
            {
                Name = m.Id(me.Name),
                Params = [.. me.Params.Select(p => p with { Name = m.Id(p.Name), Type = p.Type?.Map(m) })],
                Body = me.Body.Map(m),
                Row = me.Row is null ? null : me.Row with
                {
                    Effects = [.. me.Row.Effects.Select(e => e.Map(m))],
                    Tails = [.. me.Row.Tails.Select(t => t.Map(m))],
                },
            },
            Export e => e with { Of = e.Of.Map(m) },
            SyntaxDecl s => s with { Name = m.Id(s.Name), Role = m.MapRole(s.Role) },
            Instantiate i => i with { Instantiation = m.MapInstantiation(i.Instantiation) },
            Hole h => h with { Name = m.Id(h.Name) },
            Effect e => e with
            {
                Name = m.Id(e.Name),
                Params = [.. e.Params.Select(m.Id)],
                Ops = [.. e.Ops.Select(o => o with { Input = o.Input.Map(m), Output = o.Output.Map(m) })],
            },
            Trait t => t with { Name = m.Id(t.Name), Param = m.Id(t.Param), Fields = [.. t.Fields.Select(f => (f.Name, f.Type.Map(m)))] },
            Impl i => i with
            {
                Name = i.Name is null ? null : m.Id(i.Name),
                TraitPath = i.TraitPath.Map(m),
                Arg = i.Arg.Map(m),
                Fields = i.Fields is { } fields ? [.. fields.Select(f => (f.Name, f.Value.Map(m)))] : null,
            },
            Macro ma => ma with { Name = m.Id(ma.Name), Value = ma.Value.Map(m), Output = ma.Output?.Map(m) },
            MacroCall c => c with { Head = c.Head.Map(m), Args = [.. c.Args.Select(m.MapCapture)] },
            _ => throw new NotImplementedException($"not ported yet: a syntax traversal over the binding {GetType().Name}"),
        };
        return m.Binding(mapped);
    }

    /// <summary>The names a declaration binds -- not the ids it refers to.</summary>
    public Binding MapBinders(Func<Id, Id> f) => this switch
    {
        Let l => l with { Name = f(l.Name) },
        RecGroup g => g with { Members = [.. g.Members.Select(x => x with { Name = f(x.Name) })] },
        Method me => me with { Name = f(me.Name) },
        SyntaxDecl s => s with { Name = f(s.Name) },
        Macro ma => ma with { Name = f(ma.Name) },
        _ => this,
    };
}

public abstract partial record Pattern
{
    public Pattern Map(SyntaxMapper m)
    {
        Pattern mapped = this switch
        {
            Wild or Atom => this,
            Bind b => b with { Name = m.Id(b.Name) },
            Prod p => p with { Items = [.. p.Items.Select(i => i.Map(m))] },
            Or o => o with { Left = o.Left.Map(m), Right = o.Right.Map(m) },
            Con c => c with { Head = c.Head.Map(m), Args = [.. c.Args.Select(a => a.Map(m))] },
            AtomType or SynonymParam => this,
            Record r => r with { Type = r.Type.Map(m), Fields = [.. r.Fields.Select(f => (f.Name, f.Pattern.Map(m)))] },
            StructType st => st with { Fields = [.. st.Fields.Select(f => (f.Name, f.Pattern.Map(m)))] },
            _ => throw new NotImplementedException($"not ported yet: a syntax traversal over the pattern {GetType().Name}"),
        };
        return m.Pattern(mapped);
    }
}

public abstract partial record TokenTree
{
    public TokenTree Map(SyntaxMapper m) => this switch
    {
        Leaf l => l with { Token = m.MapToken(l.Token) },
        Group g => g with { Items = [.. g.Items.Select(i => i.Map(m))] },
        _ => throw new InvalidOperationException($"unhandled token tree {GetType().Name}"),
    };
}
