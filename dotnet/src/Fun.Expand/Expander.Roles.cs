using Fun.Kernel;

namespace Fun.Expand;

public sealed partial class Expander
{
    /// <summary>Every intro scope an application minted here.</summary>
    private readonly HashSet<int> _introScopes = [];

    /// <summary>Reads forms with the roles bound so far (M9).</summary>
    private IDisposable Reading() => Enforest.Reading(EnforestEnv.Lazy(_bindings));

    /// <summary>
    /// M7: a syntactic role never mixes with another binder of its name where
    /// both are visible. A new binder written with scope set
    /// <paramref name="occurrence"/> conflicts with an existing binder of the
    /// other sort whose scope set is a subset of it -- unless the scopes the new
    /// binder has beyond it include an intro scope (an application wrote it, and
    /// hygiene keeps the two apart), or it is a fixity-only role attaching to a value.
    /// </summary>
    private void CheckRoleMixing(string name, ScopeSet occurrence, bool isRole, bool attaches, bool group)
    {
        if (group) return;
        foreach (var existing in _bindings.Candidates(name))
        {
            if (existing.IsGroup || (existing.Kind == BinderMeaning.Role) == isRole) continue;
            if (!existing.Scope.IsSubsetOf(occurrence)) continue;
            if (occurrence.Except(existing.Scope).Values.Any(_introScopes.Contains)) continue;
            if (attaches && existing.Kind == BinderMeaning.Value) continue;
            throw new ExpandException($"`{name}` is both a syntactic role and a value binder where both are visible");
        }
    }

    /// <summary>A syntax form or fixity declaration as a binder: the forms read after it resolve its role by scope set.</summary>
    private ScopeSet BindRole(Id name, Role role)
    {
        CheckRoleMixing(name.Name, name.Scope, isRole: true, role.Attaches, role.Meaning is RoleMeaning.OrderGroup);
        var scope = FreshScope();
        _bindings.Extend(name.Name, name.Scope.Union(scope), name.Name, BinderMeaning.Role, role);
        return scope;
    }

    // ---- application --------------------------------------------------------

    /// <summary>
    /// The one hygiene contract of an application (M2): what it receives gets a
    /// fresh use-site scope and a fresh intro scope; what it returns has the
    /// intro scope flipped, so ids it received lose it again and ids the rule
    /// wrote gain it -- neither side can capture the other's.
    /// </summary>
    private sealed record Application(SyntaxMapper Receive, SyntaxMapper Emit, SyntaxMapper PruneUseSite);

    private Application NewApplication()
    {
        var useSite = _scopeCounter++;
        var intro = _scopeCounter++;
        _introScopes.Add(intro);
        return new Application(
            SyntaxMapper.OfIds(id => id with { Scope = id.Scope.Add(useSite).Add(intro) }),
            SyntaxMapper.OfIds(id => id with { Scope = id.Scope.Contains(intro) ? id.Scope.Remove(intro) : id.Scope.Add(intro) }),
            SyntaxMapper.OfIds(id => id with { Scope = id.Scope.Remove(useSite) }));
    }

    /// <summary>A syntax form's use in expression position: its replacement filled with the captures, expanded in place.</summary>
    private Syntax ExpandInstantiate(Syntax.Instantiate use)
    {
        var app = NewApplication();
        var captures = Receive(app, use.Instantiation);
        if (use.Instantiation.Rule.Replacement is not Replacement.Expr expr)
            throw new ExpandException($"syntax form {use.Instantiation.Form.Name} returns declarations where an expression goes");
        return Expand(expr.Syntax.Map(Fill(captures)).Map(app.Emit));
    }

    /// <summary>A declaration syntax form's use: the declarations it returns, ready to be bound where it was written.</summary>
    private EquatableArray<Binding> InstantiateDecls(Instantiation inst)
    {
        var app = NewApplication();
        var captures = Receive(app, inst);
        if (inst.Rule.Replacement is not Replacement.Decls decls)
            throw new ExpandException($"syntax form {inst.Form.Name} returns an expression where declarations go");
        var fill = Fill(captures);
        return [.. SpliceDeclHoles(captures, decls.Bindings.Select(b => b.Map(fill))).Select(b =>
        {
            var flipped = b.Map(app.Emit);
            // A declaration returned into a definition context binds for the rest
            // of it, so its binders lose the use-site scope (Flatt 2016); unread
            // items do too, on every token, whichever turns out a binder.
            return flipped is Binding.Items ? flipped.Map(app.PruneUseSite) : flipped.MapBinders(app.PruneUseSite.Id);
        })];
    }

    private static Dictionary<string, Capture> Receive(Application app, Instantiation inst) =>
        inst.Captures.ToDictionary(c => c.Hole, c => app.Receive.MapCapture(c.Capture));

    // ---- filling ------------------------------------------------------------

    private static string? HoleOf(string name) => name is ['$', _, ..] ? name[1..] : null;

    /// <summary>
    /// Fills a replacement with what its rule's holes captured (M9). A hole is an
    /// id spelled <c>$x</c>: in expression position it takes the capture itself,
    /// as a name the captured identifier, as a pattern the captured pattern, as
    /// an item the captured declarations, and as a <c>{ … }</c> the captured block.
    /// A rule the replacement declares binds its own holes, which are left alone.
    /// </summary>
    private static SyntaxMapper Fill(IReadOnlyDictionary<string, Capture> captures)
    {
        Capture? Find(string name) => HoleOf(name) is { } hole && captures.TryGetValue(hole, out var c) ? c : null;
        ExpandException Unfit(string what, string name) => new($"the hole {name} does not fit {what}");

        Rule FillRule(SyntaxMapper _, Rule rule)
        {
            var inner = RuleHoles(rule.Pattern).ToHashSet();
            return SyntaxMapper.MapRuleDefault(Fill(captures.Where(c => !inner.Contains(c.Key)).ToDictionary()), rule);
        }

        return new SyntaxMapper
        {
            Id = id => Find(id.Name) is Capture.Id c ? SyntaxMapper.TokenId(c.Token) : id,
            Token = token => token.Kind is TokenKind.Ident i && Find(i.Name) is Capture.Id c ? c.Token with { Span = token.Span } : token,
            Form = form => form switch
            {
                Syntax.Var v => Find(v.Id.Name) switch
                {
                    Capture.Expr e => e.Syntax,
                    Capture.Block b => new Syntax.Block(b.Terms, v.Span),
                    null or Capture.Id => form,
                    _ => throw Unfit("an expression", v.Id.Name),
                },
                Syntax.Block { Terms: [TokenTree.Leaf { Token.Kind: TokenKind.Ident i }] } block => Find(i.Name) switch
                {
                    Capture.Block b => block with { Terms = b.Terms },
                    Capture.Expr e => e.Syntax,
                    null => form,
                    _ => throw Unfit("a { … } body", i.Name),
                },
                Syntax.Module m => m with { Bindings = SpliceDeclHoles(captures, m.Bindings) },
                Syntax.Struct s => s with { Bindings = SpliceDeclHoles(captures, s.Bindings) },
                _ => form,
            },
            Binding = binding => binding is Binding.Items { Terms: [TokenTree.Leaf { Token.Kind: TokenKind.Ident i }] } items
                ? Find(i.Name) switch
                {
                    Capture.Block b => items with { Terms = b.Terms },
                    null => binding,
                    _ => throw Unfit("a { … } body", i.Name),
                }
                : binding,
            Pattern = pattern => pattern is Pattern.Bind b
                ? Find(b.Name.Name) switch
                {
                    Capture.Pattern p => p.Value,
                    null or Capture.Id => pattern,
                    _ => throw Unfit("a pattern", b.Name.Name),
                }
                : pattern,
            Rule = FillRule,
            UsedRule = FillRule,
        };
    }

    private static EquatableArray<Binding> SpliceDeclHoles(IReadOnlyDictionary<string, Capture> captures, IEnumerable<Binding> bindings) =>
        [.. bindings.SelectMany(b => b is Binding.Hole h && HoleOf(h.Name.Name) is { } hole && captures.TryGetValue(hole, out var c)
            ? c switch
            {
                Capture.Decls d => d.Bindings,
                Capture.Decl d => [d.Binding],
                _ => throw new ExpandException($"the hole {h.Name.Name} does not fit a declaration"),
            }
            : [b])];

    private static IEnumerable<string> RuleHoles(IEnumerable<RulePart> parts) => parts.SelectMany(p => p switch
    {
        RulePart.Hole h => [h.Name],
        RulePart.Group g => RuleHoles(g.Parts),
        _ => [],
    });

    // ---- blocks -------------------------------------------------------------

    /// <summary>
    /// A block whose first statement uses a declaration syntax form: its
    /// declarations scope over the rest of the block, which is read after them.
    /// </summary>
    private Syntax? ExpandBlockDeclForm(Syntax.Block block)
    {
        (Instantiation, Terms)? use;
        using (Reading()) use = Enforest.BlockDeclForm(new Terms(block.Terms));
        if (use is not var (inst, rest)) return null;

        Syntax body = rest.IsEmpty ? new Syntax.Atom(Atom.Unit.Instance, block.Span) : new Syntax.Block(rest.ToArray(), rest.Span);
        var decls = InstantiateDecls(inst).SelectMany(BlockDecls).ToList();
        for (var i = decls.Count - 1; i >= 0; i--) body = DeclOver(decls[i], body);
        return Expand(body);
    }

    /// <summary>A block's declarations as the private declarations that scope over the rest: unread items are read.</summary>
    private IEnumerable<Binding> BlockDecls(Binding binding)
    {
        if (binding is not Binding.Items items) return [binding];
        var read = new List<Binding>();
        var terms = new Terms(items.Terms);
        while (!Enforest.DropSeparators(terms).IsEmpty)
        {
            var (stmt, after) = Enforest.TakeStatement(terms);
            using (Reading()) read.AddRange(Enforest.ParseModuleStatement(stmt));
            terms = after;
        }
        return read.SelectMany(BlockDecls);
    }

    /// <summary>A declaration in a block, as the form that scopes it over the rest.</summary>
    private static Syntax DeclOver(Binding binding, Syntax body) => binding switch
    {
        Binding.Let { Public: false } l => new Syntax.Let(l.Name, null, l.Value, body, l.Recursive, body.Span),
        Binding.RecGroup { Public: false } g => new Syntax.LetRecGroup(g.Members, body, body.Span),
        Binding.SyntaxDecl { Public: false } s => new Syntax.SyntaxDef(s.Name, s.Role, body, body.Span),
        Binding.Open o => new Syntax.Open(o.Of, body, o.Label, body.Span),
        _ => throw new ExpandException("a declaration syntax form in a block writes only private lets, opens and syntax"),
    };

    /// <summary><c>pub</c> on a declaration form's use publishes every declaration it returns.</summary>
    private static Binding Publish(Binding binding) => binding switch
    {
        Binding.Let l => l with { Public = true },
        Binding.RecGroup g => g with { Public = true },
        Binding.Method m => m with { Public = true },
        Binding.SyntaxDecl s => s with { Public = true },
        Binding.Instantiate i => i with { Public = true },
        _ => binding,
    };
}
