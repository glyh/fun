namespace Fun.Kernel;

/// <summary>
/// A macro's signature (macro-annotation-constraints-mean-nothing): the type it has
/// as a function over types -- its type binders, then the <c>T</c> of each
/// <c>(x : Expr(T))</c> parameter, then the type its output promises -- written as a
/// pi type so it elaborates where the macro is defined. <paramref name="Binders"/> are
/// the macro's own type binders as written; <paramref name="Params"/> names each
/// explicit parameter and says whether the signature has a domain for it.
/// </summary>
public sealed record MacroSignature(Syntax Type, EquatableArray<string> Binders, EquatableArray<(string Name, bool Typed)> Params);

public abstract partial record Syntax
{
    /// <summary>
    /// <c>macro name(params) [: Expr(T) | : Decl | : List(Decl)] { body }</c> scoped over
    /// the rest of a block. <paramref name="Kind"/> is the annotation as written;
    /// <paramref name="Output"/> the type it promises, when it names one.
    /// </summary>
    public sealed record MacroDef(Id Name, Syntax Value, Syntax Body, FormKind? Kind, Syntax? Output, SourceSpan Span)
        : Syntax(Span);

    /// <summary>
    /// A procedural macro's call, its arguments read as its parameters' kinds (M9).
    /// After expansion only a type-aware macro's call is left, for the elaborator:
    /// its head is the macro's resolved name and each expression argument is
    /// wrapped in <see cref="Stx"/>.
    /// </summary>
    public sealed record MacroCall(Syntax Head, EquatableArray<Capture> Args, SourceSpan Span) : Syntax(Span);

    /// <summary>
    /// <c>quote(…)</c>: syntax written literally in a macro body. Each hole <c>$x</c>
    /// stands in <paramref name="Template"/> as an id spelled <c>"$x"</c> -- <c>$</c>
    /// cannot begin a source identifier -- and in <paramref name="Holes"/> as the
    /// ordinary reference <c>x</c>.
    /// </summary>
    public sealed record Quote(Syntax Template, EquatableArray<(string Hole, Syntax Value)> Holes, SourceSpan Span) : Syntax(Span);

    /// <summary><c>quote { … }</c>: declarations written literally, holes as in <see cref="Quote"/>.</summary>
    public sealed record QuoteDecls(EquatableArray<Binding> Items, EquatableArray<(string Hole, Syntax Value)> Holes, SourceSpan Span)
        : Syntax(Span);

    /// <summary>
    /// A type-aware macro's expression argument, travelling as syntax: expansion
    /// leaves it alone, and the elaborator elaborates it where the call was written.
    /// </summary>
    public sealed record Stx(Syntax Inner, SourceSpan Span) : Syntax(Span);

    /// <summary>
    /// A typed macro argument placed by the macro's output where the call's
    /// elaborator already elaborated it: <paramref name="Arg"/> names that result,
    /// <paramref name="Form"/> is the argument as expanded at the call. Written only
    /// after the macro returns, so expansion leaves it alone; a macro handed it
    /// receives <paramref name="Form"/>.
    /// </summary>
    public sealed record Elaborated(int Arg, Syntax Form, SourceSpan Span) : Syntax(Span);
}

public abstract partial record Binding
{
    /// <summary><c>[pub] macro name(params) [: annotation] { body }</c> as a module or struct item.</summary>
    public sealed record Macro(Id Name, Syntax Value, bool Public, FormKind? Kind, Syntax? Output) : Binding;

    /// <summary>
    /// A declaration macro's call <c>f(args)</c> in item position. <paramref name="Public"/>
    /// makes every declaration it returns public.
    /// </summary>
    public sealed record MacroCall(Syntax Head, EquatableArray<Capture> Args, bool Public) : Binding;
}
