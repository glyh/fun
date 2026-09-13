# fun

`fun` is an experimental dependently-typed language. Source becomes a `Core`
term through enforestation, macro expansion and bidirectional elaboration, and a
term becomes a value through normalisation by evaluation.

This glossary covers the **elaborate ↔ evaluate boundary** — the vocabulary a
port must reproduce — and, in the phases section, the reader, enforestation
and expansion phases before it. The effects section follows the decided model —
where the current implementation departs from it (bare arrows, dynamic
handling, untracked mutation), the implementation is the defect, not the
glossary.

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
_Avoid_: procedural macro (redundant — a template is sugar for one), transformer,
syntax extension

**Macro annotation**:
The `: …` after a macro's parameters. It fixes the macro kind and, for an
`Expr` kind, may name the type the output must have. Every name in it is a
reference; a macro binds type parameters the way a function does, in `[…]`.
_Avoid_: macro signature, return annotation

**Provisional macro**:
A macro whose name is registered before its body is compiled, so the name
exists during its own definition. A self-call in that window is a loud error;
after the definition, recursion works by re-expansion, under the evaluation
budget.
_Avoid_: stub, forward reference, placeholder

**Type-aware macro**:
A macro whose annotation names a type, resolved against what was elaborated
before the macro's definition. Only the annotation is type-aware; expanding a
call stays syntactic. The call is deferred to the elaborator — the arguments
travel as syntax objects — and the output is expanded in place, like any
macro's.
_Avoid_: typed macro, elaborator macro

**Syntax object**:
The macro-visible tree: a form, its span, and identifiers carrying scope sets.
What macros inspect and build; what expansion consumes.
_Avoid_: AST, syntax tree, Surface

**Reflection**:
The ADTs a macro sees syntax through — one constructor per form, covering the
whole grammar. Total: nothing is opaque; what today rides as an undecomposed
`StxExpr` is scaffolding, not a boundary.
_Avoid_: macro AST, syntax value, wrap/unwrap view

**Round trip**:
Taking syntax apart through reflection and building it back. Must be the
identity — name, span, scope, and every payload field survive. A macro that
merely reflects and rebuilds changes nothing.
_Avoid_: wrap/unwrap, conversion, serialization

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
A pattern→replacement rewrite declared by `syntax` or `pub infix` — sugar for a
macro. The template keeps one job of its own, the parse: its patterns and
fixity decide which tokens a use consumes and what each hole captures. The
rest *is* a macro: its arguments are the captures, and its body is the
replacement as quoted syntax, so ids written in it resolve where the template
was *defined*. One contract, because one path.
_Avoid_: syntax macro, pattern macro, syntax-rules

**Quoted syntax**:
Syntax written literally inside a macro body, as `quote(…)`. Its ids carry the
scopes of where the macro was defined, so they resolve there — the hygienic way
a macro refers to a name. It is parsed where it is written, so its shape is
fixed there too — no caller's operator or syntax form can reach inside it. It
may contain holes, `$e`, each splicing a syntax object the macro computed, which
keeps its own scopes; a hole's kind is fixed by its position in the parse, and
splicing a value of another kind is an error. A template's replacement is
quoted syntax.
_Avoid_: template (that is a whole rewrite plus a parse), literal, gensym

**Hole**:
A named place in a template's pattern or in quoted syntax. In a pattern it
captures a syntax object; in quoted syntax it splices one. Its kind is a
reflection type — `Expr`, `Pattern`, `Decl` or `Id` — and nothing else: the
kinds a template captures are the parameter types of the macro it is sugar
for. Whether an `Id` binds or refers is decided by where it is spliced, not by
its kind.
_Avoid_: capture kind, binder hole, ident hole, metavariable

**Borrowed context**:
Building an id whose scope set is copied from another syntax object — the one
deliberate way to break hygiene. Not a separate operation: an id is a value like
any other, and borrowing is constructing one with another id's scope set. An id
built from a name alone has an empty scope set, and is unbound unless a binder
with no scopes exists.
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

**Meta**:
An unknown term the elaborator solves by unification, standing for a function of
the bound entries in scope where it was created.
_Avoid_: hole, unification variable, flex

**Defined entry**:
An entry whose value is known. A meta skips over these rather than abstracting
over them.
_Avoid_: transparent, concrete, let-bound

**Slot**:
What a binding contributes to a context: an ordered list, one item per entry it
adds, each with a name where it has one. Elaboration and evaluation both read
the same list, so they cannot disagree on order or count.
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

**Open**:
A binding that brings a module's public members into scope as bare names, in
order, so they shadow earlier names and later bindings shadow them. Delivers
macros and syntactic roles the same way it delivers values. Which members a
module has may be known only from its type, so expansion does not enumerate
them: a bare name inside an open's region that resolves to no binder, or only
to one outside the open, resolves to an **open choice** - the opens between it
and its binder, innermost first, then the binder. Elaboration takes the first
open that has the member. Scope sets decide which opens are candidates; no
name is ever found by its spelling alone.
_Avoid_: import (that reaches a unit), include, using

**Open choice**:
What expansion resolves a bare name to when an open might supply it: the
ordered candidate opens, and the binder to fall back to, if any. Settled
during elaboration, when the opened modules' types are known.
_Avoid_: dynamic scope, fallback lookup, spelling tier

**Binding width**:
How many entries a single binding adds to the context. Known from the binding for
every kind except `open`, whose width is the number of public members in the
opened module's type — known at elaboration, like every other width.
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

**Scope set** (as a value):
What an id carries in reflection. Opaque: a macro can move one from syntax it
received or quoted into an id it builds, but can neither construct nor inspect
one — so every scope set a macro holds came from the definition site or the
use site, and hygiene is a guarantee rather than a convention.
_Avoid_: scope list, scope id, context

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
The kind of form a macro returns — a reflection type, such as `Expr` or
`Decl`, the same types a hole can have — fixed by its return type. A use is well-formed only where the expansion position matches;
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

### Effects

**Effect family**:
A declared set of operations, possibly parameterised (`effect State(S) = sig …
end`). A nominal: its identity follows the same rule, so `State(I64)` is one
effect wherever it is written.
_Avoid_: effect type, effect interface, ability

**Effect row**:
The set of effects a function may perform when called, written after `can`.
Closed (`can {IO, State(I64)}`), open with a tail variable (`can {IO | r}`), or
inferred (`can _`). An arrow with no row is pure: its row is `{}`.
_Avoid_: effect set, effect list, ability (Frank's word)

**Heap**:
A region of the mutable store, named by a type-level variable — the `h` in
`Read(h)`. Every reference lives on one, and each mutation effect is
parameterised by the heap it acts on. A definition's locally allocated refs get
a fresh one, which is what lets discharge tell an internal ref from an escaping
one.
_Avoid_: store (that is all mutable state, not one region), region (region
inference's word — lifetimes, not effects), world

**Reference**:
A mutable cell. `Ref(A)` abbreviates `Ref(h, A)`: every reference is branded by
its heap, and the brand is what discharge checks — a reference escaping its
definition carries its heap into the result type, and the effect survives; one
that stays inside lets the heap be generalised away.
_Avoid_: pointer, box, mutable variable

**Mutation effect**:
One of the three heap effects — `Alloc(h)`, `Read(h)`, `Write(h)` — naming what
is done to a heap, not merely that something was: read-only code earns the
weaker row. There is no merged `Mut`, and `Ref` is not an effect — it is the
type.
_Avoid_: Mut(h) (considered and rejected — the split), can Ref (that name is the
type's), heap effect (that is all three), side effect

**Discharge**:
Dropping a definition's heap effects for a heap that cannot escape — one that
does not occur in the definition's type — at generalisation, so internal-only
refs leave a pure signature. runST's soundness condition, met by inference
instead of a wrapper.
_Avoid_: mask (Koka's written `mask` is a different thing), purify, erase

**Pure**:
Having the empty effect row. What an unannotated arrow means, what lets the
checker evaluate a call while type checking — within the evaluation budget —
and what makes a nominal declared in the call applicative.
_Avoid_: total (pure code may diverge), side-effect free

**Evaluation budget**:
How many semantic steps the checker may spend evaluating while type checking —
function calls, loop iterations, and macro applications, which are calls.
Exceeding it is a compile error naming the call, raisable per evaluation.
Termination is never checked; divergence is not an effect.
_Avoid_: fuel (the retired depth guard's name), termination check,
reduction depth

**Handler**:
A `match` with effect branches, handling the effects its scrutinee performs
directly — not effects that tunnel through it from a function passed in, which
belong to that function's row and reach the handler that row names. Deep: it
stays installed for the resumed continuation.
_Avoid_: catch, try, effect handler block

**Residual row**:
The row that survives a handler: the effects its branches name are removed;
everything else — an open tail included — passes through unchanged. An
unhandled effect is one that flows on with the residual until some handler
names it.
_Avoid_: leftover effects, remaining effects

**Handler scope**:
The region where a handler's effects may be performed. A stored continuation may
be resumed after its branch returns — resuming re-enters the scope. A closure
whose row names a handled effect may not leave the scope; that is a compile
error, like an escaping existential.
_Avoid_: dynamic extent, handler lifetime

**Accidental handling**:
A handler catching an effect it could not see in any type — an effect raised by
a callback whose row the handling code is polymorphic over. Ruled out: such
effects tunnel.
_Avoid_: effect capture, handler leak

**Continuation**:
The rest of a handled computation, up to its handler, passed to an effect
branch and continued with `resume`. One-shot: resumed at most once. Never
captured across an extern frame.
_Avoid_: call/cc (that is undelimited), multi-shot continuation, callback
