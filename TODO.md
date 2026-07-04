# TODO

## Bugs (high)

### Disambiguate annotation names by scope

Currently `known_type_names` in `enforest.ml` is a hardcoded list:
`["I64"; "Bool"; "Unit"; "Char"; "String"; "Type"; "Id"; "Span"; "Expr";
  "Option"; "List"; "AtomVal"; "Explicitness"; "Ref"; "Pattern"; "Decl"; "Trait"]`

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
- Replace `known_type_names` only after that model exists.
- Later tests should cover user-defined, aliased, and imported type names in
  `: Expr(...)` annotations.

### Audit hardcoded symbol names

There are hardcoded string names scattered across the codebase — type names
(`"Type"`, `"R"`), constructor names (`"RExpr"`, `"Some"`, `"None"`), module
paths (`"Syntax"`), etc. These should be centralized or replaced with nominal
identity references where possible. Places to audit:

- `enforest.ml`: `"Syntax"`, `"R"` (R-type annotation), `"Type"` (old annotation)
- `elab_infer/elab_check.ml`: `"RExpr"` constructor name for wrapping
- `elab_resolve.ml`: `"stdlib"` module name
- `elab_prelude.ml`: stdlib source code has many hardcoded names
- `macro_eval.ml`, `expand.ml`, `expand_ctx.ml`: syntax nominal references

**Decision**: this audit is about semantic correctness, not cosmetic constants.
Do not compare type/nominal identity by string when a resolved value or nominal
id is available. A small helper/constant is fine when it removes a duplicated
compiler invariant, but broad prelude/test string centralization is not the goal
of this bug-fix pass.

### Private type visibility (design)

**Status**: separate task. Do not implement as part of the current concrete bug
fix pass. Track the design in `docs/16.private_type_visibility.md`.

## Features (medium)

### Stage 11: Macro-Powered Language Features (no spec yet)

### Stage 12: Macro Diagnostics & Expansion UX (no spec yet)
