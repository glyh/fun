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
public sealed class Expander
{
    private readonly BindingTable _bindings = new();
    private int _scopeCounter;
    private int _resolvedNameCounter;

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

    public Syntax Expand(Syntax stx)
    {
        switch (stx)
        {
            case Syntax.Atom:
                return stx;

            case Syntax.Var v:
                // An unbound name stays as written: the elaborator reports it
                // against the base context, which knows the primitives.
                return _bindings.Resolve(v.Id) is { } info
                    ? v with { Id = v.Id with { Name = info.ResolvedName } }
                    : stx;

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
                throw new ExpandException($"not ported yet: expanding {stx.GetType().Name}");
        }
    }

    private static Id Rename(Id name, ScopeSet scope, string resolved) =>
        name with { Name = resolved, Scope = name.Scope.Union(scope) };
}
