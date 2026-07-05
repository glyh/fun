---
status: resolved
label: wayfinder:grilling
resolved_at: 2026-07-05
resolution: resolved/closed
resolution_doc: docs/17.type_aware_macro_interleaving_design.md
blockers: []
blocks: []
---

# Type-aware macro interleaving handshake

## Question

Decide the minimal sequential expand/elaborate handshake needed to fix
annotation-name disambiguation, without prematurely designing a full
task-queue expander.

## Context

- `TODO.md` tracks this as a high-priority bug.
- `docs/plan-for-macros/TYPE_AWARE_INTERLEAVING.md` describes the long-term
  type-aware interleaving model.
- The current static `known_type_names` mechanism is wrong because annotation
  name resolution changes macro arity: an unresolved uppercase name introduces
  an implicit `Syntax.R` binder, while a resolved type/value name is a
  constraint and introduces no binder.
- Parser/expander-only sets are insufficient because they cannot see all
  elaboration-context facts that should affect this arity decision.

## Resolution

**Resolved.** Design completed — see
[docs/17.type_aware_macro_interleaving_design.md](../../../17.type_aware_macro_interleaving_design.md).

### Summary of decisions

- **Design-only.** No implementation source changes yet.
- **No OCaml 5 effects.** Explicit first-order queue/state machine — portable to C#.
- **New semantic module driver** near `elab_driver` / `Core_loader` boundary.
  Not in `Elab_infer` or `Expand`.
- **Deterministic ordered pass.** No suspension/defer queue; one binding at a time.
- **Parser produces unresolved syntax only.** No semantic macro decisions at
  parse time.
- **Split `MacroAnnotation.t` (unresolved) from `MacroKind.t` (resolved sum
  type: `ExprAny | ExprBinder | ExprConstraint of resolved_type_ref`).**
- **Annotation resolution:** `Expr(Foo)` is a constraint only if `Foo` is a
  type in the current prior type namespace. Unresolved uppercase names remain
  binders. Typo detection deferred to future warning/lint.
- **Imports** are ordered driver steps that extend the type namespace and
  macro registry.
- **Macro definitions** compile under current prior semantic context only.
  Final output is macro-free.
- **`macro_exports`** are explicit first-class driver output with resolved
  kind, captured compile-time environment, hygiene metadata, and module
  identity/version.
- **Imported macros** use the same driver pipeline; `visit_macros` separate
  path is rejected.
- **Macro-generated declarations** re-enter the same queue item-by-item in
  order, with hygiene metadata preserved.
- **Fail-stop** on first error; no best-effort recovery.
- **Call-time expansion remains syntactic.** Type awareness limited to
  annotation resolution at definition/export time.
- **Self-recursive macros** supported via provisional placeholder + rollback.
  No mutual recursion yet. Explicit fuel/stack guard.
- **Cache identity** based on resolved macro exports, module identity/source
  digest, macro name, resolved kind, and prior semantic environment fingerprint.
- **Staged migration** (9 stages): AST split → remove `known_type_names` →
  driver skeleton → annotation resolver → canonical registration → generated
  re-entry → recursive cells + fuel → unified imports → retire old paths.
- **Extensive acceptance tests** covering prior/later type resolution,
  imports, cache invalidation, generated declaration ordering, hygiene spans,
  macro-free output, and diagnostic hooks.

Tickets 02, 03, and 07 are no longer blocked by this ticket.
