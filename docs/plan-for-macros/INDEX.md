# Macro reference catalog

This directory collects references for a future `fun` macro system. It keeps the original PDFs, but the Klister repository has been reduced to selected commentary, examples, and implementation notes so future searches do not have to wade through the full source tree.

## Design summary

- [`SUMMARY.md`](SUMMARY.md) — synthesized design summary for `fun`: hygienic regular-syntax macros, type-aware/type-providing macros, stuck expansion, phases, and compiler-structure impact.
- [`IMPLEMENTATION_PLAN.md`](IMPLEMENTATION_PLAN.md) — staged implementation path for building the macro system without committing to parser or expansion dead ends.
- [`STAGE_7_ENFORESTATION_PLAN.md`](STAGE_7_ENFORESTATION_PLAN.md) — concrete implementation plan for Stage 7 enforestation and regular syntax extension.

## Papers

### Regular syntax and enforestation

- [`papers/regular-syntax/honu-enforestation.pdf`](papers/regular-syntax/honu-enforestation.pdf)
  - Honu: syntactic extension for algebraic notation through enforestation.
  - Relevant for preserving regular syntax instead of forcing Lisp-style macro syntax.
  - Look here for operators, precedence, syntax classes, and macro-driven grouping/parsing.

### Hygiene and scope sets

- [`papers/hygiene/binding-as-sets-of-scopes.pdf`](papers/hygiene/binding-as-sets-of-scopes.pdf)
  - Racket's scope-set hygiene model.
  - Relevant for identifiers as syntax objects with scope sets, subset-based resolution, generated identifiers, and intentional hygiene control.

### Type-integrated macros

- [`papers/type-integrated-macros/type-systems-as-macros.pdf`](papers/type-integrated-macros/type-systems-as-macros.pdf)
  - Turnstile-style type systems as macros.
  - Relevant for macros that check/provide types, type environments, and typed embedded languages.

### DSL extensibility

- [`papers/dsl-extensibility/macros-for-domain-specific-languages.pdf`](papers/dsl-extensibility/macros-for-domain-specific-languages.pdf)
  - Macro-extensible DSLs that inherit host-language macro infrastructure.
  - Relevant for custom languages/sublanguages sharing syntax objects, scope, modules, and IDE/compiler support.

## Extracted Klister reference

### Overview and bibliography

- [`extracted/klister/README.rst`](extracted/klister/README.rst)
  - Klister feature overview: hygienic macros, custom languages, syntax objects, phase-aware modules, type-aware/type-providing/stuck/problem-aware macros.
- [`extracted/klister/bibliography.rst`](extracted/klister/bibliography.rst)
  - Bibliographic pointers from Klister.

### Commentary

- [`extracted/klister/commentary/architecture.md`](extracted/klister/commentary/architecture.md)
  - Expansion architecture, task queue, split core, macro monad, identifier resolution, module loading/visiting.
- [`extracted/klister/commentary/scope-sets.md`](extracted/klister/commentary/scope-sets.md)
  - Worked explanation of scope-set hygiene.
- [`extracted/klister/commentary/interleaving.md`](extracted/klister/commentary/interleaving.md)
  - Worked explanation of interleaving macro expansion and typechecking with blocked tasks.

### Implementation notes

- [`extracted/klister/source-notes/implementation-map.md`](extracted/klister/source-notes/implementation-map.md)
  - Selected source excerpts and architecture map from Klister's `Syntax`, `ScopeSet`, `Phase`, and `Expander` modules.

### Representative examples

- [`extracted/klister/examples/which-problem.kl`](extracted/klister/examples/which-problem.kl)
  - Problem-aware macros: declaration/type/expression/pattern expansion; expected-type-driven macro behavior.
- [`extracted/klister/examples/type-eq.kl`](extracted/klister/examples/type-eq.kl)
  - Type-case and macro-level type equality.
- [`extracted/klister/examples/datatype-macro.kl`](extracted/klister/examples/datatype-macro.kl)
  - Datatypes as macro abstraction.
- [`extracted/klister/examples/hygiene.kl`](extracted/klister/examples/hygiene.kl)
  - Hygiene behavior tests.
- [`extracted/klister/examples/bound-identifier.kl`](extracted/klister/examples/bound-identifier.kl)
  - Bound identifier comparison.
- [`extracted/klister/examples/free-identifier-case.kl`](extracted/klister/examples/free-identifier-case.kl)
  - Free identifier comparison and identifier-dispatching macros.
- [`extracted/klister/examples/implicit-conversion.kl`](extracted/klister/examples/implicit-conversion.kl)
  - Type-directed expansion for implicit conversions.
- [`extracted/klister/examples/custom-module.kl`](extracted/klister/examples/custom-module.kl)
  - Macro/module integration.
- [`extracted/klister/examples/meta-macro.kl`](extracted/klister/examples/meta-macro.kl)
  - Macros that generate or interact with macro bindings.
- [`extracted/klister/examples/quasiquote-syntax-test.kl`](extracted/klister/examples/quasiquote-syntax-test.kl)
  - Quasiquote/unquote syntax construction and splicing.

## Quick lookup by topic

- Regular syntax: `papers/regular-syntax/`, Honu.
- Hygiene: `papers/hygiene/`, `extracted/klister/commentary/scope-sets.md`, `examples/hygiene.kl`.
- Type-aware macros: `extracted/klister/commentary/interleaving.md`, `examples/which-problem.kl`, `examples/implicit-conversion.kl`.
- Type-providing/type-system macros: `papers/type-integrated-macros/`, `examples/type-eq.kl`.
- Phases/modules: `extracted/klister/commentary/architecture.md`, `source-notes/implementation-map.md`, `examples/custom-module.kl`.
- DSL inheritance: `papers/dsl-extensibility/`.
