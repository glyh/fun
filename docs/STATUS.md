# STATUS — canonical current implementation snapshot

This is the **authoritative** status document for the `fun` compiler prototype.
When other docs disagree with this file, STATUS.md wins.

Last updated: after Type-aware macro interleaving Stages 8–9 (July 2026).

---

## Completed

### Regression coverage
- Imports, module files, records, record patterns, methods, `self`/`Self`, qualified
  patterns/constructors, algebraic effects (`perform`, handlers, `resume`) all have
  regression test coverage. See [regression coverage](wayfinder/topics/regression-coverage.md).

### Type-case / generic programming
- Primitive and nominal type-head matching, structural record type reflection,
  open-`Type` fallback, generic equality dispatch.
  See [type-case / generic programming](wayfinder/topics/type-case-generic-programming.md).

### Record type reflection
- `struct … end` type patterns over constructor fields.
  See [record type reflection](wayfinder/topics/record-type-reflection.md).

### Algebraic effects
- Nominal effect families, latent rows with open row tails, `perform`, match-based
  handlers, `resume`. Deep handler semantics, lexical resume in nested lambdas,
  one-shot continuations. See [algebraic effects](wayfinder/topics/algebraic-effects.md).

### References
- `Ref(A)`, `ref(e)`, `deref(r)`, `r <- e`. Opaque mutable cells, aliasing and
  closure-capture semantics preserved. See [references](wayfinder/topics/references.md).

### Macro system — Stages 0–10
- Stages 0 through 10 are complete: substrate, hygiene, expansion, phase-aware imports,
  enforestation, syntax templates, computed ADT-based syntax API, kind-tagged macros,
  Decl/Pattern ADTs, type-aware macros. See [macro status](wayfinder/macro-system/STATUS.md).
- The Stage 10 annotation-name disambiguation limitation is resolved on the
  semantic driver path: annotations are resolved against the current prior
  type namespace (builtins, user types, value aliases, and qualified
  imported types via `Expr(M.T)`). See
  [type-aware interleaving](wayfinder/macro-system/TYPE_AWARE_INTERLEAVING.md).
- Type-aware interleaving migration Stages 1–9 are done: AST split, static
  list removal, `Macro_driver` skeleton, prelude-type constraint resolution,
  canonical per-binding kind registration via injected callback,
  macro-generated declaration re-entry (generated `MacroBinding` nodes
  compile/register, generated siblings thread scopes), scoped per-binding
  semantic advancement (top-level source-order prior user type/record
  declarations now constrain later macro annotations), recursive-macro
  safety infrastructure (top-level provisional macro registration/rollback plus
  depth-style macro expansion fuel shared across copied contexts),
  driver-based import loading (`Macro_driver.visit_macros` compiles imported
  public macros through a full driver run, so their annotations resolve in
  the imported module's own context), and retirement of the old
  `Core_loader.visit_macros` parser-heuristic path. Same-Decl generated
  type→macro interleaving and transformer-level self-recursive macro
  bodies are deferred.

---

## Mostly complete / in progress

### Traits and trait stdlib/pub semantics
- Trait declarations, `impl` declarations, structural dictionary evidence,
  trait-bound implicit parameters, qualified method calls, public module/struct
  impl evidence all implemented.
- Remaining: explicit deriving/fallback behavior as library-level type-case code;
  more protocol-style operations. See [traits](wayfinder/topics/traits.md) and
  [trait module/stdlib](wayfinder/topics/trait-module-stdlib.md).

---

## Active / deferred

### Macro Stages 11–12
- Stage 11 (macro-powered language features) and Stage 12 (macro diagnostics &
  expansion UX) have no specification yet. See [macro status](wayfinder/macro-system/STATUS.md)
  and [macro implementation plan](wayfinder/macro-system/IMPLEMENTATION_PLAN.md).

### Annotation scope disambiguation / type-aware interleaving
- Migration Stages 1–9 are done; semantic annotation resolution is active for
  driver-based module compilation and for imported macro modules. Remaining
  deferred items: same-Decl generated type→macro interleaving,
  transformer-level self-recursive macro bodies, mutually recursive macro
  groups, semantic `resolved_type_ref` constraint identity (constraints are
  still recorded by name and resolved at the use site), and resolved-export
  cache fingerprinting (the macro cache is still keyed by module path). See
  [macro interleaving design](wayfinder/topics/macro-interleaving-design.md),
  [type-aware interleaving](wayfinder/macro-system/TYPE_AWARE_INTERLEAVING.md),
  and the [direction map](wayfinder/fun-design-map.md).

### Private type visibility
- Design-only task using the OCaml/SML model (private types become abstract outside
  their defining module). See [private type visibility](wayfinder/topics/private-type-visibility.md)
  and the [direction map](wayfinder/fun-design-map.md).

### Generated symbol cleanup
- Trait declaration markers, struct nominal hashes, and other compiler-internal
  generated symbols should be replaced with explicit structural representations.
  See [generated symbol cleanup](wayfinder/topics/generated-symbol-cleanup.md).

### Enforester improvements
- Structured errors with spans, fault-tolerant parsing, spec-oriented combinators.
  See [enforester improvements](wayfinder/topics/enforester-improvements.md).
