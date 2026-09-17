using Fun.Kernel;

namespace Fun.Expand;

/// <summary>
/// Expansion: reads a block's statements one form at a time, mints a scope for
/// every binder, and renames each occurrence to the binder it resolves to. What
/// comes out has no <see cref="Syntax.Block"/> left in it and every
/// <see cref="Syntax.Var"/> carries a resolved name.
/// </summary>
public sealed partial class Expander
{
    private BinderTable _bindings = new();
    private int _scopeCounter;
    private int _resolvedNameCounter;

    /// <summary>Every open entered so far: the scope it adds to its region, and its label.</summary>
    private readonly List<(int Scope, string Label)> _opens = [];

    public static Syntax ExpandExpr(string source, IMacroRuntime runtime, string? file = null, bool openPrelude = false) =>
        new Expander(runtime).Expand(Enforest.ParseExpr(source, file, openPrelude));

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
        CheckRoleMixing(name.Name, name.Scope, isRole: false, attaches: false, group: false);
        var scope = FreshScope();
        var resolved = FreshResolvedName(name.Name);
        _bindings.Extend(name.Name, name.Scope.Union(scope), resolved);
        return (scope, resolved);
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
        var region = _opens
            .Where(o => v.Id.Scope.Contains(o.Scope) && (binder is null || !binder.Scope.Contains(o.Scope)))
            .OrderByDescending(o => o.Scope)
            .Select(o => o.Label);
        // An id no binder took, introduced by a form imported from a unit, may
        // also mean that unit's names.
        var units = binder is not null
            ? []
            : v.Id.Scope.Values.Where(_introScopeUnits.ContainsKey).Select(s => UnitOpenLabel(_introScopeUnits[s]));
        var opens = region.Concat(units).Distinct().ToEquatableArray();
        return binder is not null && opens.IsEmpty
            ? v with { Id = v.Id with { Name = binder.ResolvedName } }
            : new Syntax.OpenChoice(v.Id, opens, binder?.ResolvedName);
    }

    public Syntax Expand(Syntax stx)
    {
        switch (stx)
        {
            case Syntax.Atom or Syntax.OpenChoice or Syntax.Self or Syntax.SelfType:
                return stx;

            // An import loads its unit's syntax wherever it is written; its roles
            // bind only in the region of the open or binder that imported it.
            case Syntax.Import import:
                _runtime.LoadSyntax(import.Path);
                return stx;

            case Syntax.Var v:
                return ResolveOccurrence(v);

            case Syntax.Open o:
            {
                var of = Expand(o.Of);
                var (scope, label) = EnterOpen(o.Of);
                ImportRoles(of, Occurrence(o.Of), scope, opened: true);
                var body = Expand(o.Body.AddScope(scope));
                return o with { Of = of, Body = body, Label = label, RolesInRegion = RolesInRegion(label) };
            }

            case Syntax.Module m:
                return m with { Bindings = ExpandBindings(m.Bindings) };

            case Syntax.Match m: return ExpandMatch(m);
            case Syntax.EffectDef or Syntax.Perform or Syntax.Resume: return ExpandEffects(stx)!;
            case Syntax.RefNew n: return n with { Arg = Expand(n.Arg) };
            case Syntax.RefGet g: return g with { Ref = Expand(g.Ref) };
            case Syntax.RefSet r: return r with { Ref = Expand(r.Ref), Value = Expand(r.Value) };
            case Syntax.TraitDef or Syntax.ImplDef or Syntax.TraitBoundSet: return ExpandTraits(stx);

            // Constructor names are labels of the type, not binders.
            case Syntax.Enum e: return e with { Constructors = [.. e.Constructors.Select(c => c with { Payloads = [.. c.Payloads.Select(Expand)] })] };
            case Syntax.PatternSynonym s: return ExpandPatternSynonym(s);

            case Syntax.LetRecGroup g:
                return ExpandLetRecGroup(g);
            case Syntax.Struct st:
                return st with { Bindings = ExpandBindings(st.Bindings) };

            case Syntax.Sig sg:
                return sg with { Bindings = ExpandBindings(sg.Bindings) };

            case Syntax.RecordConstruct r:
                return r with { Type = Expand(r.Type), Fields = [.. r.Fields.Select(f => (f.Name, Expand(f.Value)))] };

            case Syntax.Ap a:
                return ExpandMacroApplication(a) ?? a with { Fn = Expand(a.Fn), Arg = Expand(a.Arg) };

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
                BindImportHandle(value, l.Name, scope, resolved);
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
                return a with { Domain = Expand(a.Domain), Row = ExpandRow(a.Row, ScopeSet.Empty), Codomain = Expand(a.Codomain) };

            case Syntax.Arrow a:
            {
                // A named domain scopes over the codomain: `(n : I64) -> F(n)`.
                var domain = Expand(a.Domain);
                var (scope, resolved) = Bind(a.Name!);
                return a with
                {
                    Name = Rename(a.Name!, scope, resolved),
                    Domain = domain,
                    // The row runs the body, so it is inside the binder: `(r : Ref(I64)) ->{Mutate(r)} I64`.
                    Row = ExpandRow(a.Row, scope),
                    Codomain = Expand(a.Codomain.AddScope(scope)),
                };
            }

            case Syntax.Block b:
                // Read the body's first statement with what is bound here,
                // scoped over the rest, which stays unread until expansion
                // reaches it.
            {
                if (ExpandBlockDeclForm(b) is { } declForm) return declForm;
                Syntax head;
                head = Reader().ParseBlockHead(b.Span, new Terms(b.Terms));
                return Expand(head);
            }

            // A role binds for the rest of the block; the declaration itself elaborates to nothing.
            case Syntax.SyntaxDef d:
                return Expand(d.Body.AddScope(BindRole(d.Name, d.Role)));

            case Syntax.Instantiate use:
                return ExpandInstantiate(use);

            case Syntax.MacroDef d:
                return Expand(d.Body.AddScope(DefineMacro(d.Name, d.Value, d.Kind, d.Output, isPublic: false)));
            case Syntax.MacroCall call: return ExpandMacroCall(call);
            case Syntax.Quote or Syntax.QuoteDecls: return ExpandQuote(stx);
            // A typed macro's argument, and one its output placed where the call elaborated it: left for the elaborator.
            case Syntax.Stx or Syntax.Elaborated: return stx;

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
        // Each pending binding carries whether a `pub` form use published it.
        var pending = new Stack<(Binding Binding, bool Publish)>(bindings.Reverse().Select(b => (b, false)));
        var expanded = new List<Binding>();
        var active = ScopeSet.Empty;

        while (pending.Count > 0)
        {
            var (next, publish) = pending.Pop();
            if (publish) next = Publish(next);
            switch (next)
            {
                case Binding.Items items:
                {
                    var (stmt, after) = Enforest.TakeStatement(new Terms(items.Terms));
                    if (!Enforest.DropSeparators(after).IsEmpty) pending.Push((new Binding.Items(after.ToArray()), publish));
                    var marked = new Terms([.. stmt.Select(t => t.AddScope(active))]);
                    EquatableArray<Binding> read;
                    read = Reader().ParseModuleStatement(marked);
                    foreach (var b in read.Reverse()) pending.Push((b, publish));
                    break;
                }

                // A role binds for the items after it, which carry its scope.
                case Binding.SyntaxDecl decl:
                {
                    decl = (Binding.SyntaxDecl)decl.AddScope(active);
                    active = active.Union(BindRole(decl.Name, decl.Role));
                    if (decl.Public) _syntaxExports.Add((decl.Name.Name, decl.Role));
                    break;
                }

                case Binding.Instantiate use:
                {
                    var returned = InstantiateDecls(((Binding.Instantiate)use.AddScope(active)).Instantiation);
                    foreach (var b in returned.Reverse()) pending.Push((b, publish || use.Public));
                    break;
                }

                case Binding.Hole hole:
                    throw new ExpandException($"an unfilled declaration hole {hole.Name.Name}");

                // A macro binds for the items after it; it contributes no member.
                case Binding.Macro macro:
                {
                    macro = (Binding.Macro)macro.AddScope(active);
                    active = active.Union(DefineMacro(macro.Name, macro.Value, macro.Kind, macro.Output, macro.Public));
                    break;
                }

                case Binding.MacroCall call:
                {
                    var returned = ApplyDeclMacro((Binding.MacroCall)call.AddScope(active));
                    foreach (var b in returned.Reverse()) pending.Push((b, publish || call.Public));
                    break;
                }

                case Binding.Let l:
                {
                    l = (Binding.Let)l.AddScope(active);
                    var (scope, resolved) = Bind(l.Name);
                    var value = Expand(l.Recursive ? l.Value.AddScope(scope) : l.Value);
                    BindImportHandle(value, l.Name, scope, resolved);
                    expanded.Add(l with { Name = Rename(l.Name, scope, resolved), Value = value });
                    active = active.Union(scope);
                    break;
                }

                case Binding.Open o:
                {
                    o = (Binding.Open)o.AddScope(active);
                    var of = Expand(o.Of);
                    var (scope, label) = EnterOpen(o.Of);
                    ImportRoles(of, Occurrence(o.Of), scope, opened: true);
                    expanded.Add(o with { Of = of, Label = label });
                    active = active.Union(scope);
                    break;
                }

                case Binding.RecGroup g:
                    active = ExpandRecGroupBinding(g, active, expanded);
                    break;

                case Binding.Effect e:
                {
                    var (scope, effect) = ExpandEffectBinding((Binding.Effect)e.AddScope(active));
                    expanded.Add(effect);
                    active = active.Union(scope);
                    break;
                }
                case var declared when declared is Binding.Trait or Binding.Impl:
                    active = ExpandTraitBinding(declared, active, expanded);
                    break;

                case Binding.Export e:
                {
                    var export = ExpandExport(e, active);
                    ExportUnitRoles((Binding.Export)e.AddScope(active));
                    expanded.Add(export);
                    break;
                }

                // A field is a label, not a binder: nothing after it sees it.
                case Binding.Field f:
                    expanded.Add(f with { Type = Expand(f.Type.AddScope(active)) });
                    break;

                case Binding.Method m:
                {
                    var (scope, method) = ExpandMethod((Binding.Method)m.AddScope(active));
                    expanded.Add(method);
                    active = active.Union(scope);
                    break;
                }

                case var other:
                    throw new NotImplementedException($"not ported yet: expanding the binding {other.GetType().Name}");
            }
        }
        // An open's region is the rest of the list: what it may not supply is known now.
        return [.. expanded.Select(b => b is Binding.Open o ? o with { RolesInRegion = RolesInRegion(o.Label) } : b)];
    }

    private static Id Rename(Id name, ScopeSet scope, string resolved) =>
        name with { Name = resolved, Scope = name.Scope.Union(scope) };
}
