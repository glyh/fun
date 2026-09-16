namespace Fun.Kernel;

public enum Fixity { Prefix, Infix }

public enum Assoc { Left, Right, None }

/// <summary>Where a syntax form or macro may be used: an expression or a declaration position.</summary>
public enum FormKind { Expr, Decl }

public enum OrderRelation { Stronger, Weaker, Same, Unrelated }

/// <summary>
/// An order group: precedence is relative. A group is its declaration --
/// <paramref name="Group"/> is unique -- and carries the groups its declaration
/// names, so two groups compare wherever their roles travel.
/// </summary>
/// <param name="Weakest">Weaker than every group that states no relation to it.</param>
public sealed record Order(
    string Group, string Name, Assoc Assoc, bool Weakest,
    EquatableArray<Order> StrongerThan, EquatableArray<Order> WeakerThan)
{
    /// <summary>
    /// How <paramref name="a"/> relates to <paramref name="b"/>: the transitive
    /// closure of the relations their declarations, and the ones those name,
    /// state. A stated relation wins; otherwise a weakest group is weaker than
    /// one that is not.
    /// </summary>
    public static OrderRelation Relation(Order a, Order b)
    {
        var nodes = new Dictionary<string, Order>();
        void Collect(Order o)
        {
            if (!nodes.TryAdd(o.Group, o)) return;
            foreach (var n in o.StrongerThan.Concat(o.WeakerThan)) Collect(n);
        }
        Collect(a);
        Collect(b);

        // The groups x is declared directly stronger than, by either side.
        IEnumerable<string> Below(string x) => nodes.Values.SelectMany(n =>
            n.Group == x ? n.StrongerThan.Select(o => o.Group)
            : n.WeakerThan.Any(o => o.Group == x) ? [n.Group]
            : []);

        bool Reaches(string from, string target)
        {
            var seen = new HashSet<string>();
            var pending = new Stack<string>([from]);
            while (pending.Count > 0)
            {
                var x = pending.Pop();
                if (x == target) return true;
                if (!seen.Add(x)) continue;
                foreach (var y in Below(x)) pending.Push(y);
            }
            return false;
        }

        if (a.Group == b.Group) return OrderRelation.Same;
        if (Reaches(a.Group, b.Group)) return OrderRelation.Stronger;
        if (Reaches(b.Group, a.Group)) return OrderRelation.Weaker;
        if (a.Weakest && !b.Weakest) return OrderRelation.Weaker;
        if (b.Weakest && !a.Weakest) return OrderRelation.Stronger;
        return OrderRelation.Unrelated;
    }
}

/// <summary>What a binder means to the enforester.</summary>
public abstract partial record RoleMeaning
{
    /// <summary>Fixity only: a use calls the value of its name.</summary>
    public sealed record ApplyValue : RoleMeaning { public static readonly ApplyValue Instance = new(); }

    /// <summary><c>&lt;-</c>.</summary>
    public sealed record AssignRef : RoleMeaning { public static readonly AssignRef Instance = new(); }

    /// <summary>A use applies the procedural macro of its name.</summary>
    public sealed record CallMacro : RoleMeaning { public static readonly CallMacro Instance = new(); }

    /// <summary>A syntax form: rules that match tokens and fill a quoted replacement (M9).</summary>
    public sealed record Rules(FormKind Kind, EquatableArray<Rule> Items) : RoleMeaning;

    /// <summary>An order group's name; the role's <see cref="Role.Order"/> is its declaration.</summary>
    public sealed record OrderGroup : RoleMeaning { public static readonly OrderGroup Instance = new(); }

    /// <summary><c>~&gt;</c>: an arrow whose effects are polymorphic.</summary>
    public sealed record PolyArrow : RoleMeaning { public static readonly PolyArrow Instance = new(); }
}

/// <summary>
/// A syntactic role (M7): what a binder means to the enforester, resolved by
/// scope set like any binder. <paramref name="FromUnit"/> is the unit an
/// imported role came from.
/// </summary>
public sealed record Role(Fixity Fixity, Order? Order, RoleMeaning Meaning, SourceSpan DeclaredAt, string? FromUnit)
{
    /// <summary>A fixity-only role attaches to the value of its name rather than binding a new one.</summary>
    public bool Attaches => Meaning is RoleMeaning.ApplyValue;
}

/// <summary>
/// One rule: the tokens a use consumes and what each hole captures, and the
/// replacement -- quoted syntax parsed where the rule is written.
/// </summary>
public sealed record Rule(EquatableArray<RulePart> Pattern, Replacement Replacement, SourceSpan Span);

public abstract partial record RulePart
{
    public sealed record Literal(TokenTree Term) : RulePart;
    public sealed record Group(Delimiter Delimiter, EquatableArray<RulePart> Parts, SourceSpan Span) : RulePart;
    public sealed record Hole(string Name, HoleKind Kind, SourceSpan Span) : RulePart;
}

/// <summary>What a hole captures: the reflection types (M10).</summary>
public enum HoleKind { Expr, Block, Id, Decls, Decl, Pattern, Tokens }

public abstract partial record Replacement
{
    public sealed record Expr(Syntax Syntax) : Replacement;
    public sealed record Decls(EquatableArray<Binding> Bindings) : Replacement;
}

/// <summary>What a hole captured at a use.</summary>
public abstract partial record Capture
{
    public sealed record Expr(Syntax Syntax) : Capture;

    /// <summary>A <c>{ … }</c> group, captured unread.</summary>
    public sealed record Block(EquatableArray<TokenTree> Terms) : Capture;

    public sealed record Id(Token Token) : Capture;
    public sealed record Pattern(Fun.Kernel.Pattern Value) : Capture;
    public sealed record Decls(EquatableArray<Binding> Bindings) : Capture;

    /// <summary>Exactly one declaration.</summary>
    public sealed record Decl(Binding Binding) : Capture;

    /// <summary>The rest of a declaration use, unread.</summary>
    public sealed record Tokens(EquatableArray<TokenTree> Terms) : Capture;
}

/// <summary>
/// A syntax form's use: the rule that matched and what its holes captured.
/// <paramref name="FromUnit"/> is the unit the form was imported from.
/// </summary>
public sealed record Instantiation(Id Form, Rule Rule, EquatableArray<(string Hole, Capture Capture)> Captures, string? FromUnit);

public abstract partial record Syntax
{
    /// <summary>A syntax form or fixity declaration scoped over the rest of a block (M7).</summary>
    public sealed record SyntaxDef(Id Name, Role Role, Syntax Body, SourceSpan Span) : Syntax(Span);

    /// <summary>A syntax form's use, filled and expanded like a macro application (M9).</summary>
    public sealed record Instantiate(Instantiation Instantiation, SourceSpan Span) : Syntax(Span);
}

public abstract partial record Binding
{
    /// <summary>
    /// A syntax form or fixity declaration, as the binder it is: the forms after
    /// it are read with its role, and a value binder of its name visible with it
    /// is an error (M7).
    /// </summary>
    public sealed record SyntaxDecl(Id Name, Role Role, bool Public) : Binding;

    /// <summary>A declaration syntax form's use. <paramref name="Public"/> publishes every declaration it returns.</summary>
    public sealed record Instantiate(Instantiation Instantiation, bool Public) : Binding;

    /// <summary>A declaration hole <c>$d</c> in quoted items, filled at instantiation.</summary>
    public sealed record Hole(Id Name) : Binding;
}
