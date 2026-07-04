# TODO

## Bugs (high)

### Clarified bug-fix scope

- Fix every concrete bug in this section.
- Treat **Private type visibility** as a separate feature/design task, not part
  of this bug-fix pass.
- For unclear TODOs, prefer semantic fixes over cosmetic cleanup. In
  particular, do not merely centralize hardcoded strings if the real issue is
  comparing values/types/nominals by name.

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

### Nominal identity vs. name comparisons

Any code that compares nominals by `String.equal` on names is fragile because
nominals can be aliased through rebinds (`type T = SomeNominal`). The nominal's
`id` field exists for identity comparison and should be used instead of name
comparison wherever possible.

**Decision**: this also applies to pattern matching. Constructor lookup may use
constructor names to select a field within an already-known nominal, but it must
not use type/nominal names to decide identity when aliases/rebinds are possible.

**Implementation target**:

- Audit `find_nominal_template_opt`, `find_nominal_for_constructor`,
  `unqualified_constructor_in_scope`, `elab_patterns.ml`, `elab_match.ml`, and
  `elab_refine.ml`.
- Prefer the scrutinee nominal id when elaborating constructor patterns.
- Preserve qualified constructor visibility checks, but reject wrong-nominal
  matches by nominal id rather than by name.
- Add/adjust tests for module aliases, nested-module ADTs, and pattern synonyms
  over module-scoped ADTs.

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

### Nested-module ADT constructor resolution

`pub pattern PatWild = RawPatWild(_)` inside modules can fail because constructor
resolution traverses by type name, not constructor name. Affects pattern matching
in macro bodies for module-scoped ADTs. The `find_nominal_template_opt` and
related code in `elab_patterns.ml` and `elab_resolve.ml` need attention.

**Implementation target**:

- Resolve constructor patterns by constructor value/type where possible,
  including qualified paths through modules and aliases.
- For type-case patterns, use constructor-name lookup only to find a constructor
  within a resolved nominal, not to infer nominal identity from a matching type
  name.
- Add regression coverage for public pattern synonyms inside modules over
  module-scoped ADTs.

### Private type visibility (design)

**Status**: separate task. Do not implement as part of the current concrete bug
fix pass. Track the design in `docs/16.private_type_visibility.md`.

## Features (medium)

### Stage 11: Macro-Powered Language Features (no spec yet)

### Stage 12: Macro Diagnostics & Expansion UX (no spec yet)
