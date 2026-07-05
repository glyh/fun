# TODO

For detailed current implementation status, see [`docs/STATUS.md`](docs/STATUS.md).

## Bugs (high)

### Disambiguate annotation names by scope

Stage 2 of the type-aware interleaving migration removed the old static
compiler-known list (`Compiler_names.Type_name.macro_annotation_known`). Stage 3
added an additive callable `Macro_driver` skeleton, but it intentionally preserves
the current whole-module expansion behavior. Macro annotations still use a
temporary parser/enforester fallback: `_` is unconstrained, leading-uppercase
names become binders with synthesized implicit R parameters, and lowercase/
non-binder names are unconstrained.

This means neither builtin names nor user-defined types are resolved as
constraints yet: `: Expr(I64)`, `: Expr(MyTag)`, and misspellings such as
`: Expr(Intt)` are all binders until the semantic module driver can resolve
annotation names against the current prior type namespace.

**Decision**: elaboration decides, but this requires an expander/elaborator
handshake because binder-vs-constraint changes macro arity. Do not fix this with
a parser name set or an expander-only type-name set. Track the design in
`docs/plan-for-macros/TYPE_AWARE_INTERLEAVING.md`.

**Implementation target**:

- Stage 1 (AST split), Stage 2 (remove static known-type list), and Stage 3
  (additive callable `Macro_driver` skeleton) are complete.
- Next: implement the semantic resolver path that decides binder vs constraint
  using the current prior elaboration context.
- Future regressions are listed in
  `docs/plan-for-macros/TYPE_AWARE_INTERLEAVING.md`.

### Private type visibility (design)

**Status**: separate task. Do not implement as part of the current concrete bug
fix pass. Track the design in `docs/16.private_type_visibility.md`.

## Features (medium)

### Stage 11: Macro-Powered Language Features (no spec yet)

### Stage 12: Macro Diagnostics & Expansion UX (no spec yet)
