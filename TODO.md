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

### Nominal identity vs. name comparisons

Any code that compares nominals by `String.equal` on names is fragile because
nominals can be aliased through rebinds (`type T = SomeNominal`). The nominal's
`id` field exists for identity comparison and should be used instead of name
comparison wherever possible.

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

### Nested-module ADT constructor resolution

`pub pattern PatWild = RawPatWild(_)` inside modules can fail because constructor
resolution traverses by type name, not constructor name. Affects pattern matching
in macro bodies for module-scoped ADTs. The `find_nominal_template_opt` and
related code in `elab_patterns.ml` and `elab_resolve.ml` need attention.

### Private type visibility (design)

**Decision**: OCaml/SML path. Private types can leak through public bindings
but become abstract outside the module. Values can be passed around; constructors
are rejected unqualified.

**Unification needed**: `pub`/`private` on module entries and constructor
visibility are the same concern (module-boundary access control), but currently
handled in two unrelated places:

1. `open_module_value` / `add_opened_field` — registers pub names into the
   name table during `open`
2. `elab_patterns.ml:239` / `unqualified_constructor_in_scope` — checks the
   name table to decide if a constructor is usable unqualified

These should be one mechanism: `open_module_value` registers both the type and
its constructors for public VNominals (recursively into sub-modules). Private
types never register, so their constructors are naturally unreachable.
`unqualified_constructor_in_scope` can then be dropped from `elab_patterns.ml`
entirely — lookup in the name table IS the access check.

**Implementation plan**: make `open_module_value` recursively register public
sub-module ADT constructors into the name table. Then move the VNominal
constructor-list check before the `unqualified_constructor_in_scope` guard in
`elab_patterns.ml:239` (the name table already encodes public vs private).
Revert `imports 10` test to `import_elab_fail`.

**Cross-language consensus** (10 languages): private type constructors never
usable unqualified after `open`/`import`.

## Features (medium)

### Stage 11: Macro-Powered Language Features (no spec yet)

### Stage 12: Macro Diagnostics & Expansion UX (no spec yet)
