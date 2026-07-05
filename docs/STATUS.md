# STATUS — canonical current implementation snapshot

This is the **authoritative** status document for the `fun` compiler prototype.
When other docs disagree with this file, STATUS.md wins.

Last updated: after Type-aware macro interleaving Stage 2 (July 2026).

---

## Completed

### Regression coverage
- Imports, module files, records, record patterns, methods, `self`/`Self`, qualified
  patterns/constructors, algebraic effects (`perform`, handlers, `resume`) all have
  regression test coverage. See [`8.regression_coverage_plan.md`](8.regression_coverage_plan.md).

### Type-case / generic programming
- Primitive and nominal type-head matching, structural record type reflection,
  open-`Type` fallback, generic equality dispatch.
  See [`9.type_case_generic_programming_plan.md`](9.type_case_generic_programming_plan.md).

### Record type reflection
- `struct … end` type patterns over constructor fields.
  See [`10.record_type_reflection_plan.md`](10.record_type_reflection_plan.md).

### Algebraic effects
- Nominal effect families, latent rows with open row tails, `perform`, match-based
  handlers, `resume`. Deep handler semantics, lexical resume in nested lambdas,
  one-shot continuations. See [`7.algebraic_effects_plan.md`](7.algebraic_effects_plan.md).

### References
- `Ref(A)`, `ref(e)`, `deref(r)`, `r <- e`. Opaque mutable cells, aliasing and
  closure-capture semantics preserved. See [`14.references_plan.md`](14.references_plan.md).

### Macro system — Stages 0–10
- Stages 0 through 10 are complete: substrate, hygiene, expansion, phase-aware imports,
  enforestation, syntax templates, computed ADT-based syntax API, kind-tagged macros,
  Decl/Pattern ADTs, type-aware macros. See [`plan-for-macros/STATUS.md`](plan-for-macros/STATUS.md).
- Stage 10 has a known limitation: annotation-name disambiguation is temporarily
  parser/enforester-local. The old static known-type list has been removed;
  uppercase names now uniformly become binders until the semantic driver can
  resolve constraints against the prior type namespace. See
  [`plan-for-macros/TYPE_AWARE_INTERLEAVING.md`](plan-for-macros/TYPE_AWARE_INTERLEAVING.md).

---

## Mostly complete / in progress

### Traits and trait stdlib/pub semantics
- Trait declarations, `impl` declarations, structural dictionary evidence,
  trait-bound implicit parameters, qualified method calls, public module/struct
  impl evidence all implemented.
- Remaining: explicit deriving/fallback behavior as library-level type-case code;
  more protocol-style operations. See [`11.trait_plan.md`](11.trait_plan.md) and
  [`12.trait_module_stdlib_plan.md`](12.trait_module_stdlib_plan.md).

---

## Active / deferred

### Macro Stages 11–12
- Stage 11 (macro-powered language features) and Stage 12 (macro diagnostics &
  expansion UX) have no specification yet. See [`plan-for-macros/STATUS.md`](plan-for-macros/STATUS.md)
  and [`plan-for-macros/IMPLEMENTATION_PLAN.md`](plan-for-macros/IMPLEMENTATION_PLAN.md).

### Annotation scope disambiguation / type-aware interleaving
- Stage 1 (AST split) and Stage 2 (remove static `known_type_names`) are done.
  Current temporary behavior maps leading-uppercase annotations to binders;
  real constraint recognition still requires expander/elaborator interleaving
  with access to the current prior type namespace. See
  [`17.type_aware_macro_interleaving_design.md`](17.type_aware_macro_interleaving_design.md),
  [`plan-for-macros/TYPE_AWARE_INTERLEAVING.md`](plan-for-macros/TYPE_AWARE_INTERLEAVING.md),
  and [TODO.md](../TODO.md).

### Private type visibility
- Design-only task using the OCaml/SML model (private types become abstract outside
  their defining module). See [`16.private_type_visibility.md`](16.private_type_visibility.md)
  and [TODO.md](../TODO.md).

### Generated symbol cleanup
- Trait declaration markers, struct nominal hashes, and other compiler-internal
  generated symbols should be replaced with explicit structural representations.
  See [`13.generated_symbol_cleanup_plan.md`](13.generated_symbol_cleanup_plan.md).

### Enforester improvements
- Structured errors with spans, fault-tolerant parsing, spec-oriented combinators.
  See [`15.enforest_improvement_plan.md`](15.enforest_improvement_plan.md).
