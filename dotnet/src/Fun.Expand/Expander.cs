using Fun.Kernel;

namespace Fun.Expand;

/// <summary>
/// Expansion: reads a block's statements one form at a time, mints a scope for
/// every binder, and renames each occurrence to the binder it resolves to. What
/// comes out has no <see cref="Syntax.Block"/> left in it and every
/// <see cref="Syntax.Var"/> carries a resolved name.
/// </summary>
// Slice 1 has no macros, so the expander takes no IMacroRuntime yet: nothing
// here can call the elaborator. That parameter arrives with `macro`.
public sealed partial class Expander
{
    private readonly BinderTable _bindings = new();
    private int _scopeCounter;
    private int _resolvedNameCounter;

    /// <summary>Every open entered so far: the scope it adds to its region, and its label.</summary>
    private readonly List<(int Scope, string Label)> _opens = [];

    public static Syntax ExpandExpr(string source, string? file = null) =>
        new Expander().Expand(Enforest.ParseExpr(source, file));

    private ScopeSet FreshScope() => ScopeSet.Singleton(_scopeCounter++);

    /// <summary>
    /// A binder's unique name. Two binders of the same written name get
    /// different ones, so an inner binder shadows an outer one by name as well
    /// as by scope.
    /// </summary>
    private string FreshResolvedName(string name) =>
        name.Contains('#') ? name : $"{name}#{_resolvedNameCounter++}";

    /// <summary>
    /// Binds <paramref name="name"/>, returning the scope its body is marked
    /// with and the resolved name its occurrences are renamed to.
    /// </summary>
    private (ScopeSet Scope, string Resolved) Bind(Id name)
    {
        var scope = FreshScope();
        var resolved = FreshResolvedName(name.Name);
        _bindings.Extend(name.Name, name.Scope.Union(scope), resolved);
        return (scope, resolved);
    }

    /// <summary>
    /// Enters an open: a fresh scope marks its region, and a label names it so an
    /// open choice can refer to it.
    /// </summary>
    private (ScopeSet Scope, string Label) EnterOpen()
    {
        var scope = _scopeCounter++;
        var label = $"open:{scope}";
        _opens.Add((scope, label));
        return (ScopeSet.Singleton(scope), label);
    }

    /// <summary>
    /// A bare name: the binder it resolves to, or -- when an open might supply it
    /// -- an open choice. An open is a candidate when the name is inside it and
    /// its binder, if any, is not: a binder inside the open shadows it.
    /// </summary>
    private Syntax ResolveOccurrence(Syntax.Var v)
    {
        if (v.Id.Name.Contains('#')) return v;
        var binder = _bindings.Resolve(v.Id);
        var opens = _opens
            .Where(o => v.Id.Scope.Contains(o.Scope) && (binder is null || !binder.Scope.Contains(o.Scope)))
            .OrderByDescending(o => o.Scope)
            .Select(o => o.Label)
            .ToEquatableArray();
        return binder is not null && opens.IsEmpty
            ? v with { Id = v.Id with { Name = binder.ResolvedName } }
            : new Syntax.OpenChoice(v.Id, opens, binder?.ResolvedName);
    }

    public Syntax Expand(Syntax stx)
    {
        switch (stx)
        {
            case Syntax.Atom or Syntax.OpenChoice or Syntax.Import:
                return stx;

            case Syntax.Var v:
                return ResolveOccurrence(v);

            case Syntax.Open o:
            {
                var of = Expand(o.Of);
                var (scope, label) = EnterOpen();
                return o with { Of = of, Body = Expand(o.Body.AddScope(scope)), Label = label };
            }

            case Syntax.Module m:
                return m with { Bindings = ExpandBindings(m.Bindings) };

            case Syntax.LetRecGroup g:
                return ExpandLetRecGroup(g);

            case Syntax.Ap a:
                return a with { Fn = Expand(a.Fn), Arg = Expand(a.Arg) };

            case Syntax.Annotated a:
                return a with { Inner = Expand(a.Inner), Type = Expand(a.Type) };

            case Syntax.Prod p:
                return p with { Items = [.. p.Items.Select(Expand)] };

            case Syntax.ProdTy p:
                return p with { Items = [.. p.Items.Select(Expand)] };

            case Syntax.Proj p:
                return p with { Of = Expand(p.Of) };

            case Syntax.FieldAccess f:
                return f with { Of = Expand(f.Of) };

            case Syntax.Lam l:
            {
                var (scope, resolved) = Bind(l.Param.Name);
                // The body is inside the binder; the parameter's type is not.
                var body = Expand(l.Body.AddScope(scope));
                var param = l.Param with
                {
                    Name = Rename(l.Param.Name, scope, resolved),
                    Type = l.Param.Type is null ? null : Expand(l.Param.Type),
                };
                return l with { Param = param, Body = body };
            }

            case Syntax.Let l:
            {
                var (scope, resolved) = Bind(l.Name);
                // Only a `rec` binding's value is inside its own binder.
                var value = Expand(l.Recursive ? l.Value.AddScope(scope) : l.Value);
                var body = Expand(l.Body.AddScope(scope));
                return l with
                {
                    Name = Rename(l.Name, scope, resolved),
                    Type = l.Type is null ? null : Expand(l.Type),
                    Value = value,
                    Body = body,
                };
            }

            case Syntax.Arrow { Name: null } a:
                return a with { Domain = Expand(a.Domain), Codomain = Expand(a.Codomain) };

            case Syntax.Arrow a:
            {
                // A named domain scopes over the codomain: `(n : I64) -> F(n)`.
                var domain = Expand(a.Domain);
                var (scope, resolved) = Bind(a.Name!);
                return a with
                {
                    Name = Rename(a.Name!, scope, resolved),
                    Domain = domain,
                    Codomain = Expand(a.Codomain.AddScope(scope)),
                };
            }

            case Syntax.Block b:
                // Read the body's first statement with what is bound here,
                // scoped over the rest, which stays unread until expansion
                // reaches it.
                return Expand(Enforest.ParseBlockHead(b.Span, new Terms(b.Terms)));

            default:
                throw new NotImplementedException($"not ported yet: expanding {stx.GetType().Name}");
        }
    }

    /// <summary>
    /// A binding list, in order. Unread items are read one statement at a time;
    /// each binding takes the scopes of every binding before it, so it sees them,
    /// and nothing before it sees it.
    /// </summary>
    private EquatableArray<Binding> ExpandBindings(EquatableArray<Binding> bindings)
    {
        var pending = new Stack<Binding>(bindings.Reverse());
        var expanded = new List<Binding>();
        var active = ScopeSet.Empty;

        while (pending.Count > 0)
        {
            switch (pending.Pop())
            {
                case Binding.Items items:
                {
                    var (stmt, after) = Enforest.TakeStatement(new Terms(items.Terms));
                    if (!Enforest.DropSeparators(after).IsEmpty) pending.Push(new Binding.Items(after.ToArray()));
                    var marked = new Terms([.. stmt.Select(t => t.AddScope(active))]);
                    foreach (var read in Enforest.ParseModuleStatement(marked).Reverse()) pending.Push(read);
                    break;
                }

                case Binding.Let l:
                {
                    l = (Binding.Let)l.AddScope(active);
                    var (scope, resolved) = Bind(l.Name);
                    var value = Expand(l.Recursive ? l.Value.AddScope(scope) : l.Value);
                    expanded.Add(l with { Name = Rename(l.Name, scope, resolved), Value = value });
                    active = active.Union(scope);
                    break;
                }

                case Binding.Open o:
                {
                    o = (Binding.Open)o.AddScope(active);
                    var of = Expand(o.Of);
                    var (scope, label) = EnterOpen();
                    expanded.Add(o with { Of = of, Label = label });
                    active = active.Union(scope);
                    break;
                }

                case Binding.RecGroup g:
                    active = ExpandRecGroupBinding(g, active, expanded);
                    break;

                case var other:
                    throw new NotImplementedException($"not ported yet: expanding the binding {other.GetType().Name}");
            }
        }
        return [.. expanded];
    }

    private static Id Rename(Id name, ScopeSet scope, string resolved) =>
        name with { Name = resolved, Scope = name.Scope.Union(scope) };
}
