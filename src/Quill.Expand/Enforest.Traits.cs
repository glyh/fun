using Quill.Kernel;

namespace Quill.Expand;

public sealed partial class Enforest
{
    /// <summary>
    /// <c>trait Name(a) = sig { op : a -&gt; … }</c> (or <c>= module { op = a -&gt; … }</c>):
    /// its name, its one parameter and its operation types. Null when the
    /// statement is not a trait.
    /// </summary>
    private (Id Name, Id Param, EquatableArray<(string Name, Syntax Type)> Fields)? ParseTraitStatement(Terms stmt)
    {
        stmt = DropSeparators(stmt);
        if (!IsToken(stmt.Head, TokenKind.Trait)) return null;
        if (NameOf(stmt.Drop(1).Head) is not Id name) throw new ExpandException("trait declaration requires a name", stmt.Span);

        var afterName = stmt.Drop(2);
        var eq = IndexOfToken(afterName, TokenKind.Eq);
        if (eq < 0) throw new ExpandException("trait binding requires =", stmt.Span);
        var parameters = TakeTerms(afterName, eq);
        if (parameters.Count != 1 || parameters.Head is not TokenTree.Group { Delimiter: Delimiter.Paren } group)
            throw new ExpandException(parameters.IsEmpty
                ? "trait declaration requires exactly one parameter"
                : "trait parameters must be written as (A)", stmt.Span);
        RequireAdjacent(name.Span, group.Span, "trait parameter list");
        var items = DropSeparators(new Terms(group.Items));
        if (items.Count != 1 || NameOf(items.Head) is not Id param)
            throw new ExpandException(items.IsEmpty
                ? "trait declaration requires exactly one parameter"
                : "trait declaration accepts exactly one parameter", group.Span);

        return (name, param, ParseOperationTypes("trait", afterName.Drop(eq + 1)));
    }

    /// <summary>
    /// <c>sig { op : T; … }</c> or <c>module { op = T; … }</c>: each operation's type,
    /// which must be a function type.
    /// </summary>
    private EquatableArray<(string Name, Syntax Type)> ParseOperationTypes(string what, Terms terms)
    {
        terms = DropSeparators(terms);
        var isModule = IsToken(terms.Head, TokenKind.Module);
        if (!(isModule || IsToken(terms.Head, TokenKind.Sig)) || terms.Count != 2
            || terms[1] is not TokenTree.Group { Delimiter: Delimiter.Brace } body)
            throw new ExpandException($"{what} requires a module {{ … }} or sig {{ … }} block", terms.Span);

        var fields = new List<(string, Syntax)>();
        foreach (var raw in Statements(new Terms(body.Items)))
        {
            var stmt = DropSeparators(raw);
            var separator = isModule ? TokenKind.Eq : TokenKind.Colon;
            if (stmt.Head is not TokenTree.Leaf { Token.Kind: TokenKind.Ident field } || !IsToken(stmt.Drop(1).Head, separator))
                throw new ExpandException(isModule
                    ? $"expected {what} module field of the form name = Type"
                    : $"expected {what} sig field of the form name : Type", stmt.Span);
            var type = ParseAll(stmt.Drop(2));
            if (type is not Syntax.Arrow) throw new ExpandException($"{what} fields must be function types", type.Span);
            fields.Add((field.Name, type));
        }
        return [.. fields];
    }

    /// <summary>
    /// <c>impl [name] [binders] [:] Trait(Arg) = module { op = …; fn op(…) { … } }</c>: its
    /// optional name, its head binders, the trait path, its one argument and its
    /// operations. Null when the statement is not an impl.
    /// </summary>
    private (Id? Name, EquatableArray<Param> Binders, Syntax Trait, Syntax Arg, EquatableArray<(string Name, Syntax Value)> Fields)? ParseImplStatement(Terms stmt)
    {
        stmt = DropSeparators(stmt);
        if (!IsToken(stmt.Head, TokenKind.Impl)) return null;

        var afterImpl = stmt.Tail;
        var eq = IndexOfToken(afterImpl, TokenKind.Eq);
        var body = eq < 0 ? Terms.Empty : DropSeparators(afterImpl.Drop(eq + 1));
        if (eq < 0 || !IsToken(body.Head, TokenKind.Module))
            throw new ExpandException("impl binding requires = module { … }", stmt.Span);

        var (name, binders, trait, arg) = ParseImplHead(TakeTerms(afterImpl, eq));
        var (moduleBody, after) = BraceBody("module", body.Tail);
        EnsureNoRest("impl binding", after);

        var fields = new List<(string, Syntax)>();
        foreach (var field in Statements(new Terms(moduleBody.Items)))
        {
            if (ParseValueDeclStatement(field) is not var (fieldName, type, value, _))
                throw new ExpandException("expected impl let field", field.Span);
            // A written type annotates the operation, as a module binding's does.
            fields.Add((fieldName.Name, type is null ? value : new Syntax.Annotated(value, type, value.Span)));
        }
        return (name, binders, trait, arg, [.. fields]);
    }

    /// <summary><c>[name] [binders] [:] Trait(Arg)</c>: an optional impl name, its head
    /// binders (<c>[a : Eq]</c>), the trait's path and its one argument. The binders are
    /// the impl's binding form for its head variables' bounds, written before the colon.</summary>
    private (Id? Name, EquatableArray<Param> Binders, Syntax Trait, Syntax Arg) ParseImplHead(Terms terms)
    {
        terms = DropSeparators(terms);
        Id? name = null;
        EquatableArray<Param> binders = [];
        var colon = IndexOfToken(terms, TokenKind.Colon);
        if (colon >= 0)
        {
            var head = DropSeparators(TakeTerms(terms, colon));
            if (!head.IsEmpty && NameOf(head.Head) is Id written)
            {
                name = written;
                head = DropSeparators(head.Tail);
            }
            if (!head.IsEmpty && head.Head is TokenTree.Group { Delimiter: Delimiter.Bracket } binderGroup)
            {
                binders = ParseParamGroup(new Terms(binderGroup.Items), Explicitness.Implicit);
                head = DropSeparators(head.Tail);
            }
            if (!head.IsEmpty) throw new ExpandException("impl head is `impl [name] [binders] : Trait(Arg)`", head.Span);
            terms = DropSeparators(terms.Drop(colon + 1));
        }

        if (terms.Count < 2 || terms[terms.Count - 1] is not TokenTree.Group { Delimiter: Delimiter.Paren } args)
            throw new ExpandException("impl declaration requires a parenthesized trait argument", terms.Span);
        var items = DropSeparators(new Terms(args.Items));
        if (items.IsEmpty) throw new ExpandException("impl argument list cannot be empty", args.Span);
        var parts = SplitCommas(items);
        if (parts.Count != 1) throw new ExpandException("impl declaration accepts exactly one trait argument", items.Span);
        return (name, binders, ParseAll(TakeTerms(terms, terms.Count - 1)), ParseImplHeadArg(parts[0]));
    }

    /// <summary>
    /// An impl head's argument: an ordinary expression, except that a struct in
    /// it may name a rest (<c>struct { a : p; _ }</c>), which is a type pattern
    /// rather than a type. Only this position reads the rest; anywhere else a
    /// lone <c>_</c> item is still the error it was.
    /// </summary>
    private Syntax ParseImplHeadArg(Terms terms)
    {
        var outer = _allowStructRest;
        _allowStructRest = true;
        try { return ParseAll(terms); }
        finally { _allowStructRest = outer; }
    }

    /// <summary>
    /// A lone <c>_</c> item of a struct being read as an impl head's type
    /// pattern: the pattern's rest, marking the struct partial. A constructor
    /// field, the one binding a label rather than a binder, so expansion leaves
    /// it as written where a `let _` would have been renamed; its type is the
    /// unit, a syntax nothing rewrites, and what the elaborator reads it by.
    /// </summary>
    private Binding? ParseStructRest(Terms stmt)
    {
        if (!_allowStructRest) return null;
        stmt = DropSeparators(stmt);
        if (stmt is not [TokenTree.Leaf { Token.Kind: TokenKind.Ident { Name: "_" } } leaf]) return null;
        return new Binding.Field("_", new Syntax.Atom(Atom.Unit.Instance, leaf.Span));
    }

    /// <summary>A block statement declaring a trait or an impl, scoped over the rest of the block.</summary>
    private Syntax? ParseTraitOrImplStatement(SourceSpan span, Terms stmt, Syntax body)
    {
        if (ParseTraitStatement(stmt) is var (name, param, fields))
            return new Syntax.TraitDef(name, param, fields, body, span);
        if (ParseImplStatement(stmt) is var (implName, implBinders, trait, arg, implFields))
            return new Syntax.ImplDef(implName, implBinders, trait, arg, implFields, body, span);
        return null;
    }

    /// <summary>A module or struct item declaring a trait or an impl.</summary>
    private Binding? ParseTraitOrImplItem(Terms unprefixed, bool isPublic)
    {
        if (ParseTraitStatement(unprefixed) is var (name, param, fields))
            return new Binding.Trait(name, param, fields, isPublic);
        if (ParseImplStatement(unprefixed) is var (implName, implBinders, trait, arg, implFields))
            return new Binding.Impl(implName, implBinders, trait, arg, implFields, isPublic);
        return null;
    }

    /// <summary><c>name : impl Trait(Arg)</c> in a signature: the named impl the module must provide.</summary>
    private Binding ParseSignatureImpl(Terms stmt)
    {
        if (NameOf(stmt.Head) is not Id name || !IsToken(stmt.Drop(1).Head, TokenKind.Colon) || !IsToken(stmt.Drop(2).Head, TokenKind.Impl))
            throw new ExpandException("an impl in a signature must be named: write name : impl Trait(Type)", stmt.Span);
        var (_, binders, trait, arg) = ParseImplHead(stmt.Drop(3));
        if (!binders.IsEmpty) throw new ExpandException("an impl in a signature does not write head binders");
        return new Binding.Impl(name, [], trait, arg, null, Public: true);
    }

    /// <summary><c>{Eq, Show}</c> after an implicit binder's colon: the traits it must implement.</summary>
    private Syntax ParseTraitBoundSet(TokenTree.Group group) =>
        new Syntax.TraitBoundSet([.. SplitCommas(DropSeparators(new Terms(group.Items))).Select(ParseAll)], group.Span);
}
