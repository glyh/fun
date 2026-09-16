namespace Fun.Kernel;

public abstract partial record Syntax
{
    /// <summary><c>match (scrutinee) { pattern => result, … }</c>: the first arm whose pattern matches.</summary>
    public sealed record Match(Syntax Scrutinee, EquatableArray<MatchBranch> Branches, SourceSpan Span) : Syntax(Span);
}

public sealed record MatchBranch(Pattern Pattern, Syntax Body)
{
    /// <summary>
    /// An effect branch's operation path (<c>effect E.op pattern =&gt; body</c>):
    /// its pattern matches the operation's argument. Null for a value branch.
    /// </summary>
    public Syntax.FieldAccess? Operation { get; init; }
}

/// <summary>A pattern as written.</summary>
// Only the pattern kinds the current slice reaches: type-case, struct-type and
// record patterns and pattern synonyms arrive later.
public abstract partial record Pattern
{
    public sealed record Wild : Pattern
    {
        public static readonly Wild Instance = new();
    }

    /// <summary>A binder: matches anything and names it for the arm's result.</summary>
    public sealed record Bind(Id Name) : Pattern;

    public sealed record Atom(Fun.Kernel.Atom Value) : Pattern;

    public sealed record Prod(EquatableArray<Pattern> Items) : Pattern;

    /// <summary><c>left | right</c>: both alternatives bind the same names.</summary>
    public sealed record Or(Pattern Left, Pattern Right) : Pattern;

    /// <summary>
    /// A constructor pattern. <paramref name="Head"/> is the path written for the
    /// constructor - a bare name or a dotted path - resolved like any expression,
    /// never by spelling.
    /// </summary>
    public sealed record Con(Syntax Head, EquatableArray<Pattern> Args) : Pattern;

    public Pattern AddScope(ScopeSet scope) => this switch
    {
        Wild or Atom => this,
        Bind b => b with { Name = b.Name with { Scope = b.Name.Scope.Union(scope) } },
        Prod p => p with { Items = [.. p.Items.Select(i => i.AddScope(scope))] },
        Or o => o with { Left = o.Left.AddScope(scope), Right = o.Right.AddScope(scope) },
        Con c => c with { Head = c.Head.AddScope(scope), Args = [.. c.Args.Select(a => a.AddScope(scope))] },
        Record r => r with { Type = r.Type.AddScope(scope), Fields = [.. r.Fields.Select(f => (f.Name, f.Pattern.AddScope(scope)))] },
        AtomType or SynonymParam => this,
        StructType s => s with { Fields = [.. s.Fields.Select(f => (f.Name, f.Pattern.AddScope(scope)))] },
        _ => throw new InvalidOperationException($"unhandled pattern {GetType().Name}"),
    };

    /// <summary>
    /// The binders the pattern writes, each name once, in source order - both
    /// alternatives of an or-pattern bind the same ones.
    /// </summary>
    public EquatableArray<Id> Binders()
    {
        var found = new List<Id>();
        void Go(Pattern p)
        {
            switch (p)
            {
                case Bind b when found.All(f => f.Name != b.Name.Name): found.Add(b.Name); break;
                case Prod pr: foreach (var i in pr.Items) Go(i); break;
                case Or o: Go(o.Left); Go(o.Right); break;
                case Con c: foreach (var a in c.Args) Go(a); break;
                case Record r: foreach (var f in r.Fields) Go(f.Pattern); break;
                case StructType s: foreach (var f in s.Fields) Go(f.Pattern); break;
            }
        }
        Go(this);
        return [.. found];
    }
}
