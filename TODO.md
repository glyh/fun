# TODO

## Bugs (high)

### Disambiguate annotation names by scope

Currently macro annotation disambiguation uses the static compiler-known list
`Compiler_names.Type_name.macro_annotation_known`.

When parsing `: Expr(Foo)`, if `Foo` is in `known_type_names`, it becomes a
constraint (`Expr(None, Some "Foo")`). Otherwise, if it starts uppercase, it
becomes a type-binding parameter (`Expr(Some "Foo", None)`).

This means a user who defines `type MyTag = I64` cannot use `: Expr(MyTag)`
as a constraint — `MyTag` is treated as a binder. The disambiguation should
be context-sensitive, tracking an environment of type names in scope rather
than a static list. This applies (at least) to all 3 sites where the implicit
R-type parameter is constructed in `enforest.ml` (~lines 308, 316, 336).

**Decision**: elaboration decides, but this requires an expander/elaborator
handshake because binder-vs-constraint changes macro arity. Do not fix this with
a parser name set or an expander-only type-name set. Track the design in
`docs/plan-for-macros/TYPE_AWARE_INTERLEAVING.md`.

**Implementation target**:

- Design the handshake/task model before changing macro arity semantics.
- Replace the static known-type list only after that model exists.
- Future regressions are listed in
  `docs/plan-for-macros/TYPE_AWARE_INTERLEAVING.md`.

### Private type visibility (design)

**Status**: separate task. Do not implement as part of the current concrete bug
fix pass. Track the design in `docs/16.private_type_visibility.md`.

## Features (medium)

### Stage 11: Macro-Powered Language Features (no spec yet)

### Stage 12: Macro Diagnostics & Expansion UX (no spec yet)
