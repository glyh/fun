using Fun.Kernel;

namespace Fun.Expand;

public static partial class Enforest
{
    /// <summary>
    /// <c>trait Name(A) = sig { op : A -&gt; … }</c> (or <c>= module { op = A -&gt; … }</c>):
    /// its name, its one parameter and its operation types. Null when the
    /// statement is not a trait.
    /// </summary>
    private static (Id Name, Id Param, EquatableArray<(string Name, Syntax Type)> Fields)? ParseTraitStatement(Terms stmt)
    {
        stmt = DropSeparators(stmt);
        if (!IsToken(stmt.Head, TokenKind.Trait)) return null;
        if (NameOf(stmt.Drop(1).Head) is not Id name) throw new ExpandException("trait declaration requires a name");

        var afterName = stmt.Drop(2);
        var eq = IndexOfToken(afterName, TokenKind.Eq);
        if (eq < 0) throw new ExpandException("trait binding requires =");
        var parameters = TakeTerms(afterName, eq);
        if (parameters.Count != 1 || parameters.Head is not TokenTree.Group { Delimiter: Delimiter.Paren } group)
            throw new ExpandException(parameters.IsEmpty
                ? "trait declaration requires exactly one parameter"
                : "trait parameters must be written as (A)");
        RequireAdjacent(name.Span, group.Span, "trait parameter list");
        var items = DropSeparators(new Terms(group.Items));
        if (items.Count != 1 || NameOf(items.Head) is not Id param)
            throw new ExpandException(items.IsEmpty
                ? "trait declaration requires exactly one parameter"
                : "trait declaration accepts exactly one parameter");

        return (name, param, ParseOperationTypes("trait", afterName.Drop(eq + 1)));
    }

    /// <summary>
    /// <c>sig { op : T; … }</c> or <c>module { op = T; … }</c>: each operation's type,
    /// which must be a function type.
    /// </summary>
    private static EquatableArray<(string Name, Syntax Type)> ParseOperationTypes(string what, Terms terms)
    {
        terms = DropSeparators(terms);
        var isModule = IsToken(terms.Head, TokenKind.Module);
        if (!(isModule || IsToken(terms.Head, TokenKind.Sig)) || terms.Count != 2
            || terms[1] is not TokenTree.Group { Delimiter: Delimiter.Brace } body)
            throw new ExpandException($"{what} requires a module {{ … }} or sig {{ … }} block");

        var fields = new List<(string, Syntax)>();
        foreach (var raw in Statements(new Terms(body.Items)))
        {
            var stmt = DropSeparators(raw);
            var separator = isModule ? TokenKind.Eq : TokenKind.Colon;
            if (stmt.Head is not TokenTree.Leaf { Token.Kind: TokenKind.Ident field } || !IsToken(stmt.Drop(1).Head, separator))
                throw new ExpandException(isModule
                    ? $"expected {what} module field of the form name = Type"
                    : $"expected {what} sig field of the form name : Type");
            var type = ParseAll(stmt.Drop(2));
            if (type is not Syntax.Arrow) throw new ExpandException($"{what} fields must be function types");
            fields.Add((field.Name, type));
        }
        return [.. fields];
    }

    /// <summary>
    /// <c>impl [name :] Trait(Arg) = module { op = …; fn op(…) { … } }</c>: its
    /// optional name, the trait path, its one argument and its operations. Null
    /// when the statement is not an impl.
    /// </summary>
    private static (Id? Name, Syntax Trait, Syntax Arg, EquatableArray<(string Name, Syntax Value)> Fields)? ParseImplStatement(Terms stmt)
    {
        stmt = DropSeparators(stmt);
        if (!IsToken(stmt.Head, TokenKind.Impl)) return null;

        var afterImpl = stmt.Tail;
        var eq = IndexOfToken(afterImpl, TokenKind.Eq);
        var body = eq < 0 ? Terms.Empty : DropSeparators(afterImpl.Drop(eq + 1));
        if (eq < 0 || !IsToken(body.Head, TokenKind.Module))
            throw new ExpandException("impl binding requires = module { … }");

        var (name, trait, arg) = ParseImplHead(TakeTerms(afterImpl, eq));
        var (moduleBody, after) = BraceBody("module", body.Tail);
        EnsureNoRest("impl binding", after);

        var fields = new List<(string, Syntax)>();
        foreach (var field in Statements(new Terms(moduleBody.Items)))
        {
            if (ParseValueDeclStatement(field) is not var (fieldName, type, value, _))
                throw new ExpandException("expected impl let field");
            // A written type annotates the operation, as a module binding's does.
            fields.Add((fieldName.Name, type is null ? value : new Syntax.Annotated(value, type, value.Span)));
        }
        return (name, trait, arg, [.. fields]);
    }

    /// <summary><c>[name :] Trait(Arg)</c>: an optional impl name, the trait's path and its one argument.</summary>
    private static (Id? Name, Syntax Trait, Syntax Arg) ParseImplHead(Terms terms)
    {
        terms = DropSeparators(terms);
        Id? name = null;
        var colon = IndexOfToken(terms, TokenKind.Colon);
        if (colon >= 0)
        {
            var nameTerms = DropSeparators(TakeTerms(terms, colon));
            if (nameTerms.Count != 1 || NameOf(nameTerms.Head) is not Id written)
                throw new ExpandException("impl name must be a single identifier");
            name = written;
            terms = DropSeparators(terms.Drop(colon + 1));
        }

        if (terms.Count < 2 || terms[terms.Count - 1] is not TokenTree.Group { Delimiter: Delimiter.Paren } args)
            throw new ExpandException("impl declaration requires a parenthesized trait argument");
        var items = DropSeparators(new Terms(args.Items));
        if (items.IsEmpty) throw new ExpandException("impl argument list cannot be empty");
        var parts = SplitCommas(items);
        if (parts.Count != 1) throw new ExpandException("impl declaration accepts exactly one trait argument");
        return (name, ParseAll(TakeTerms(terms, terms.Count - 1)), ParseAll(parts[0]));
    }

    /// <summary>A block statement declaring a trait or an impl, scoped over the rest of the block.</summary>
    private static Syntax? ParseTraitOrImplStatement(SourceSpan span, Terms stmt, Syntax body)
    {
        if (ParseTraitStatement(stmt) is var (name, param, fields))
            return new Syntax.TraitDef(name, param, fields, body, span);
        if (ParseImplStatement(stmt) is var (implName, trait, arg, implFields))
            return new Syntax.ImplDef(implName, trait, arg, implFields, body, span);
        return null;
    }

    /// <summary>A module or struct item declaring a trait or an impl.</summary>
    private static Binding? ParseTraitOrImplItem(Terms unprefixed, bool isPublic)
    {
        if (ParseTraitStatement(unprefixed) is var (name, param, fields))
            return new Binding.Trait(name, param, fields, isPublic);
        if (ParseImplStatement(unprefixed) is var (implName, trait, arg, implFields))
            return new Binding.Impl(implName, trait, arg, implFields, isPublic);
        return null;
    }

    /// <summary><c>name : impl Trait(Arg)</c> in a signature: the named impl the module must provide.</summary>
    private static Binding ParseSignatureImpl(Terms stmt)
    {
        if (NameOf(stmt.Head) is not Id name || !IsToken(stmt.Drop(1).Head, TokenKind.Colon) || !IsToken(stmt.Drop(2).Head, TokenKind.Impl))
            throw new NotImplementedException("not ported yet: an unnamed impl in a signature");
        var (_, trait, arg) = ParseImplHead(stmt.Drop(3));
        return new Binding.Impl(name, trait, arg, null, Public: true);
    }

    /// <summary><c>{Eq, Show}</c> after an implicit binder's colon: the traits it must implement.</summary>
    private static Syntax ParseTraitBoundSet(TokenTree.Group group) =>
        new Syntax.TraitBoundSet([.. SplitCommas(DropSeparators(new Terms(group.Items))).Select(ParseAll)], group.Span);
}
