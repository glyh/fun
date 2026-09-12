# fun

`fun` is an experimental dependently-typed language. Source becomes a `Core`
term through enforestation, macro expansion and bidirectional elaboration, and a
term becomes a value through normalisation by evaluation.

This glossary covers the **elaborate ↔ evaluate boundary** — the vocabulary a
port must reproduce. Surface syntax, macro expansion and effects are later
passes and are not described here yet.

## Language

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
One slot in a context, holding a value, a name, and whether it is bound or
defined.
_Avoid_: binding (that word means a module member here), variable, slot

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
is what a name resolves to.
_Avoid_: absolute index, position

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
_Avoid_: prelude, init context, global scope, root scope

**Base-anchored term**:
A term whose free indices all point into the base context. Not closed — it still
has free indices — but safe to transport, because every importer shares the same
base. This, not closedness, is the condition a cached unit must meet.
_Avoid_: closed term, ground term, standalone term

**Transport**:
Moving a *term* to a context other than its anchor. Sound only for closed terms.
Moving a *value* is always sound, because a value carries its environment with
it — which is why first-class modules work and why importing one does not.
_Avoid_: reuse, sharing, splicing, instantiation

### Modules and members

**Binding**:
A member of a module or struct — a value, a type, an effect, a pattern synonym,
an impl, or an open. What a user writes inside `module … end`.
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
required to be closed. Conflating it with a module is what made "modules must be closed"
sound like it would break first-class modules.
_Avoid_: module, file, source file, library

**Struct**:
A record type and a namespace in one construct — constructor fields alongside
bindings.
_Avoid_: record type, object, class

### Names

**Bare name**:
A name written on its own, resolved against the context. Values, types,
constructors, pattern synonyms, effect families, traits and modules all compete
here, and a later one shadows an earlier one.
_Avoid_: identifier, variable, symbol

**Member**:
A name reachable only through a container, written after a dot. Record fields,
module members, effect operations and trait methods are members. A member never
competes with a bare name — a record field named `x` does not shadow a value
named `x`.
_Avoid_: field (that is one specific kind), property, attribute

**Scope** (hygiene):
An opaque token minted for each binding form during expansion. A name occurrence
carries a *set* of them, and resolution picks the binding whose set is the
largest subset of the occurrence's. Flatt's sets-of-scopes; unrelated to the
elaboration context.
_Avoid_: context, binding scope, lexical scope

**Expander state**:
What the expander threads during expansion: the binding table keyed by scope
set, a way to run a macro, and an expansion-fuel budget. Not a context — the
elaborator borrows only the last two, and never consults the binding table.
_Avoid_: expand context, macro context, expansion context

**Expansion position**:
Whether a macro is being expanded where an expression is expected or where a
declaration is. A property of the site, not of the macro.
_Avoid_: context kind, context, macro context

**Syntactic role**:
Whether a name is an operator, or a syntax form, and with what fixity. Decided a
phase before the context exists, and keyed by name rather than by scope set — so
a later binding of that name does **not** take the role away. This is the one place
the single-namespace rule does not hold.
_Avoid_: fixity, precedence (those are parts of it), operator kind

**Nominal**:
A declared type with an identity of its own, so two nominals with the same name
are still distinct.
_Avoid_: ADT, datatype, inductive

**Constructor**:
A way of building a value of a nominal. Lives in the same namespace as
everything else, so a constructor sharing its type's name shadows that type.
_Avoid_: variant, case, tag

**Pattern head**:
The name at the front of a constructor pattern. Resolved as a type name first
and a constructor name second.
_Avoid_: pattern constructor, matcher

**Primitive**:
An operation the compiler supplies rather than the prelude: a name, a type, a
reducer, and a failure behaviour. The fourth part is what division by zero
needed and what `panic` does not fit.
_Avoid_: builtin, intrinsic, native
