# fun

`fun` is an experimental dependently-typed language. Source becomes a `Core`
term through enforestation, macro expansion and bidirectional elaboration, and a
term becomes a value through normalisation by evaluation.

This glossary covers the **elaborate ↔ evaluate boundary** — the vocabulary a
port must reproduce — and, in the phases section, the reader, enforestation
and expansion phases before it. Effects are not described here yet.

The phases before elaboration follow the macro-system design: Honu's
enforestation, Flatt's sets of scopes, and an ordered interleaving of expansion
with elaboration (borrowed from Klister, minus its suspended expansions). Where
the current implementation departs from that design, the implementation is
wrong, not the glossary.

## Language

### The phases before elaboration

**Reader**:
The pass that turns source text into tokens and delimiter groups. It decides no
forms and resolves no names. Keywords are a fixed, reserved set of token kinds
— a deliberate departure from Honu, which reserves nothing — and operators are
one uniform token shape.
_Avoid_: lexer (it also groups), parser (nothing is parsed yet)

**Group**:
A delimiter-bounded sequence of terms — the one structural notion the reader
produces. What a group becomes is decided later, by enforestation.
_Avoid_: expression, block, parentheses

**Enforestation**:
Turning a flat run of terms into a tree of forms, interleaved with expansion so
it is sensitive to bindings (Honu). Operator bindings decide form and fixity; a
name bound to a macro consumes the remaining input itself and returns a form.
_Avoid_: parsing (only a part of it), reading

**Form**:
A typed node of the syntax tree, produced by enforestation — the unit macros
return and the expander rewrites.
_Avoid_: construct, expression, statement

**Expansion**:
Running macros and templates, adding scopes to syntax objects, and resolving
each occurrence to its binder. Interleaved with enforestation, and with
elaboration one binding at a time in source order — a binding is expanded
against everything elaborated before it. Not a pass between them.
_Avoid_: resolution (only a part of it), desugaring

**Macro**:
A binder whose meaning is a function from syntax objects to a syntax object,
run during expansion when its name heads a form. Its body is elaborated in the
scope of its definition site, and nothing is ambient there.
_Avoid_: procedural macro (redundant — a template is not a macro), transformer,
syntax extension

**Macro annotation**:
The `: …` after a macro's parameters. It fixes the macro kind and, for an
`Expr` kind, may name the type the output must have. Every name in it is a
reference; a macro binds type parameters the way a function does, in `[…]`.
_Avoid_: macro signature, return annotation

**Type-aware macro**:
A macro whose annotation names a type, resolved against what was elaborated
before the macro's definition. Only the annotation is type-aware; expanding a
call stays syntactic.
_Avoid_: typed macro, elaborator macro

**Syntax object**:
The macro-visible tree: a form, its span, and identifiers carrying scope sets.
What macros inspect and build; what expansion consumes.
_Avoid_: AST, syntax tree, Surface

**Resolved name**:
The unique name expansion assigns a binder, and rewrites every occurrence
resolving to that binder to — whatever the binder's namespace. The string the
elaboration context is keyed by. Unique within a context, so locating it
involves no shadowing.
_Avoid_: unique name, generated symbol, mangled name

**Intro scope**:
A fresh scope, one per macro or template application, on the syntax that
application wrote but did not receive — so ids it introduces cannot capture the
caller's. It distinguishes; it does not bind.
_Avoid_: macro scope, use-site scope (that is its counterpart)

**Use-site scope**:
A fresh scope, one per macro or template application, on the syntax that
application received — the symmetric counterpart of the intro scope. Keeps what
one application received distinct from what another did, which recursive macros
need to avoid ambiguous references.
_Avoid_: intro scope, call-site scope

**Template**:
A pattern→replacement rewrite declared by `syntax` or `pub infix`. Holes splice
the caller's syntax objects; ids written literally in the replacement resolve
where the template was *defined*.
_Avoid_: syntax macro, pattern macro, macro (that is the function kind)

**Quoted syntax**:
Syntax written literally inside a macro body. Its ids carry the scopes of where
the macro was defined, so they resolve there — the hygienic way a macro refers
to a name.
_Avoid_: template (that is a whole rewrite), literal, quasiquote

**Borrowed context**:
Building an id from a name and another syntax object whose scope set it takes —
the one deliberate way to break hygiene. An id built from a name with no context
has an empty scope set, and is unbound unless a binder with no scopes exists.
_Avoid_: fall-through, dynamic resolution, unhygienic name

### The elaboration context

**Context**:
The ordered sequence of entries a term is elaborated and evaluated against —
type theory's Γ. Not a mapping: position is meaning, and a term is only
intelligible against the context it was built in. Seen through several columns
of equal length; no operation uses all of them.
_Avoid_: scope (that is the hygiene word), environment (one of its columns)

**Environment**:
The value column of a context: one value per entry, most recent first. The only
projection normalisation receives; it is never handed a whole context.
_Avoid_: env as a synonym for context, runtime context

**Width**:
The number of entries in a context. Every column has exactly this length, and
the elaborator and the evaluator must agree on how much each binding adds.
_Avoid_: depth, size, level count

**Entry**:
One position in a context, holding a value, a name, and whether it is bound or
defined. A slot describes an entry; it is not one.
_Avoid_: binding (that word means a module member here), variable

**Bound entry**:
An entry standing for a variable with no known value — a genuine binder. A meta
is abstracted over these.
_Avoid_: rigid, abstract, opaque

**Defined entry**:
An entry whose value is known. A meta skips over these rather than abstracting
over them.
_Avoid_: transparent, concrete, let-bound

**Slot**:
What a binding contributes to a context, stated once for both sides: an ordered
list, one item per entry it adds, each carrying a name where it has one and
where its payload comes from. The elaborator hangs a type and a value on each,
the evaluator a value; the order and the count belong to neither.
_Avoid_: contribution, field, width (that is the list's length)

**Level**:
A position counted from the outermost entry. Stable as the context grows, so it
is where a resolved name is located.
_Avoid_: absolute index, position

**Locate**:
Finding where a resolved name sits in the context (its level), or a member in
its container. Elaboration locates; expansion resolves.
_Avoid_: resolve (that is expansion's step), look up

**Index**:
A position counted back from the innermost entry. What a term stores, so it
shifts as the context grows.
_Avoid_: de Bruijn number, offset

### Terms and where they mean something

**Anchor**:
The context a term's indices are relative to. Every term has one. It is implicit
everywhere, recorded nowhere, and correct only for as long as the term stays in
the context that built it.
_Avoid_: environment, base context, enclosing scope

**Base context**:
The context every compilation unit is elaborated against: the atom types, the
primitives, and `stdlib` bound as a name. Identical for every unit, which is
what makes a unit's term safe to reuse. `stdlib` is *bound*, not *opened* — a
unit reaches the prelude qualified for free, but needs its own open for bare
prelude names and operators.
_Avoid_: prelude (that is a unit the base context binds), init context, global
scope, root scope

**Prelude**:
The `std` unit, reached through the base context's `stdlib` binding. In scope
bare only where it is opened — macro bodies included.
_Avoid_: stdlib (that is the binding), standard library, builtins

**Base-anchored term**:
A term whose free indices all point into the base context. Not closed — it still
has free indices — but safe to transport, because every importer shares the same
base. This, not closedness, is the condition a cached unit must meet.
_Avoid_: closed term, ground term, standalone term

**Transport**:
Moving a *term* to a context other than its anchor. Sound only for
base-anchored terms.
Moving a *value* is always sound, because a value carries its environment with
it — which is why first-class modules work and why importing one does not.
_Avoid_: reuse, sharing, splicing, instantiation

### Modules and members

**Binding**:
One item written inside a module or struct — a value, a type, an effect, a
pattern synonym, an impl, or an open. A named public binding is reached from
outside as a member.
_Avoid_: declaration, member, definition

**Binding width**:
How many entries a single binding adds to the context. Known from the binding for
every kind except `open`, whose width is the public-entry count of a module that
must be evaluated first.
_Avoid_: arity, size, contribution

**Module**:
A first-class value holding bindings, written `module … end` and usable wherever
a value is — passed, returned, bound. It captures the context it was written in,
like a closure. Accessed by a dotted path, which resolves to the *last* member
of that name, so a later member shadows an earlier one exactly as in a `do`
block or through `open`.
_Avoid_: namespace, record (a struct is the record-shaped thing)

**Compilation unit**:
A `.fun` file, reached by `import`. Not a module expression and not first-class:
it has no context to capture, so it is the one thing that can sensibly be
required to be base-anchored. Conflating it with a module is what made "modules
must be closed" sound like it would break first-class modules.
_Avoid_: module, file, source file, library

**Struct**:
A record type and a namespace in one construct — constructor fields alongside
bindings.
_Avoid_: record type, object, class

### Names

**Bare name**:
A name written on its own. It carries a scope set and resolves to a binder, so
hygiene governs it. Values, types, constructors, pattern synonyms, effect
families, traits and modules all compete here, and a later binder shadows an
earlier one — decided when the name is resolved, not when it is located.
_Avoid_: identifier, variable, symbol

**Member**:
A name reachable only through a container, written after a dot. Record fields,
module members, effect operations and trait methods are members. A label, not a
bare name: it carries no scopes and is resolved by its container, so hygiene
does not apply and nothing can capture it. A record field named `x` does not
shadow a value named `x`.
_Avoid_: field (that is one specific kind), property, attribute

**Binder**:
The site that introduces a name — a lambda parameter, a `let`, a type, a
module member. What a name occurrence resolves to during expansion.
_Avoid_: binding (that is a module member), declaration, definition site

**Scope** (hygiene):
An opaque token minted for each binder during expansion. A name occurrence
carries a *set* of them, and resolution picks the binder whose set is the
largest subset of the occurrence's. Flatt's sets-of-scopes; unrelated to the
elaboration context.
_Avoid_: context, binding scope, lexical scope

**Binder table**:
Every binder expansion has seen, each with its written name, scope set, resolved
name and compile-time meaning — a macro, a syntactic role, or neither. What
resolution consults. Not a context: it has no order and no width.
_Avoid_: binding table (binding is a module member), expander state, expand
context, symbol table

**Expansion position**:
Whether a macro is being expanded where an expression is expected or where a
declaration is. A property of the site, not of the macro.
_Avoid_: context kind, context, macro context

**Macro kind**:
The kind of form a macro returns — such as `Expr` or `Decl` — fixed by its
return type. A use is well-formed only where the expansion position matches;
the macro never learns its position, so one name means one kind.
_Avoid_: problem (a position the macro branches on at run time), macro type,
return kind

**Syntactic role**:
Whether a binder is an operator, or a syntax form, and with what fixity. Part of
what the binder means, resolved by scope set like any other — so a later binder
of that name shadows the role too, prelude syntax such as `if` and `not`
included. Keywords have no binder and so no role:
they are reserved, not shadowable.
_Avoid_: fixity, precedence (those are parts of it), operator kind

**Nominal**:
A declared type whose identity is its declaration together with the values of its
own free variables. Two nominals are the same type when they come from the same
declaration and those values are convertible. A declaration evaluated under a
run-time effect is generative instead: each evaluation is a new type. Which one
applies follows from purity, visible in the effect row — never declared.
_Avoid_: ADT, datatype, inductive

**Constructor**:
A way of building a value of a nominal. Lives in the same namespace as
everything else, so a constructor sharing its type's name shadows that type.
_Avoid_: variant, case, tag

**Pattern head**:
The name at the front of a constructor pattern — a bare name, resolving to a
constructor or type binder like any other.
_Avoid_: pattern constructor, matcher

**Primitive**:
An operation the compiler supplies rather than the prelude: a name, a type, a
reducer, and a failure behaviour. The fourth part is what division by zero
needed and what `panic` does not fit.
_Avoid_: builtin, intrinsic, native
