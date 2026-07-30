---
title: Unify operators into the scope-aware binding table
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
assignee: glyh
resolution: Implemented — Operator_env.t deleted; fixity is now an optional operator_info attribute on Binding.binding_info (orthogonal to Value/Macro kind). The enforester owns a Binding.t seeded by base_operators() (all builtins incl <- as BuiltinRefSet); lookup is a string-keyed newest-wins find_operator (NOT scope-based resolve). Single live shared table + operator hygiene remain deferred to the parent's interleaving work (no scopes exist in the parse phase). 787 tests green.
closed_date: 2026-07-30
blocked_by:
---

# Unify operators into the scope-aware binding table

## Question

Collapse the separate `Operator_env.t` fixity table into the one scope-aware
`binding_table`, so an operator is just a **binding that carries a fixity
attribute** — the same shape as a macro (a binding that carries a compile-time
`Macro` meaning). After this, the compiler has a **single** scope mechanism for
all compile-time meaning (values, macros, operators), and operators become
hygienic.

Split out of
[Explicit prelude open for operator demotion](explicit-prelude-open-operator-demotion.md)
as its largest and most independent piece — it is the structural precondition for
that ticket's one-table model, and the parent's remaining steps (driver-carried
delivery, explicit prelude, operator demotion) all sit on top of it.

## Context — the duplication being removed

Compile-time meaning currently lives in **two parallel structures**:

- **Macros / values** → `Expand_ctx.binding_table` (`lib/expand/expand_ctx.ml`), a
  `Binding.t` that is **scope-aware / hygienic**, tagged with a `kind`
  (`Binding.Value` | `Binding.Macro`). Resolution dispatches expand-vs-call by
  kind and honors innermost-lexical shadowing. This is where the
  `unify-macro-call-syntax-with-functions` work put macros.
- **Operators** (fixity / precedence / associativity / templates) → a **separate**
  `Operator_env.t` (`lib/expand/operator_env.ml`), a flat symbol-keyed table with
  precedence metadata. `find_infix` / `find_prefix` match by bare string; there is
  a hardcoded fallback (`infix_table` / `prefix_table`) for builtin operators.

Two tables = the same concept ("a compile-time binding later parsing depends on")
implemented twice. This is the deeper "implemented twice" behind the parent
ticket's hook, and the primary obstacle to a clean OCaml→C# model.

## Design (decided)

- **Fixity becomes a `Binding` attribute.** Extend `Binding` so an operator-kind
  binding carries fixity / precedence / associativity (and, for template
  operators, its `Syntax_template.t`) alongside the existing `Value`/`Macro` kind.
- **The precedence parser reads fixity from binding resolution**, not from
  `Operator_env`. The enforester's operator lookup and the macro resolver must
  consult **one** source of truth.
- **`Operator_env.t` is deleted.**
- **`<-` stays compiler-known — as a base-context binding, not a surviving
  table.** It is installed into the base `binding_table` with a fixity attribute
  and its `BuiltinRefSet` compile-time meaning, always in scope because it is core
  ref machinery (not stdlib). Uniform with the one-table model, just
  privileged/always-present rather than gated behind `open`.

## Scope (narrowed after planning)

Planning surfaced that **enforestation is a complete phase that finishes before
expansion begins** — there is no `Expand_ctx`/binding table alive while the parser
resolves precedence, and the parse phase has no scope sets yet. So "one table"
cannot mean *one shared live instance* here without fusing parse and expand, which
**is** the parent ticket's interleaving-driver work. This child therefore delivers:

- **In scope:** fixity becomes an optional attribute on `Binding.binding_info`
  (orthogonal to the `Value`/`Macro` kind — `(+)` is one name that is both a
  callable value and an infix operator); the enforester stores operators in a
  `Binding.t` it owns; `Operator_env.t` is deleted; operator lookup is
  string-keyed, newest-wins (a dedicated `find_operator`, **not** `Binding.resolve`,
  which returns the oldest match on equal/empty scope sets — wrong for shadowing).
- **Deferred to the parent's interleaving step:** a single live table shared across
  parse+expand, and operator **hygiene** (scope-set-keyed operator resolution).
  There is nothing to key hygiene against during the parse phase.

Decisions locked: builtins (`+ - * / % == != < > <= >=`, `not`, `<-`) are seeded
as base `Binding` entries (the `infix_table`/`prefix_table` fallback concept is
deleted; nothing is demoted, just relocated); `operator_info` lives inside
`binding.ml` (no dependency cycle); hygiene deferred.

## Implementation sequence (each step builds green)

0. Introduce the `operator_info` type + `fixity` attribute on `binding_info`
   (+ `find_operator`/`add_operator`/export helpers) in `binding.ml`; nothing
   consumes it yet. `Operator_env` still alive.
1. Switch the enforester (`enforest.ml`, `enforest_util.ml`) to read/write a
   `Binding.t` seeded by a new `base_operators ()`; rewrite the ~6 `find_*` and ~4
   `add_*` sites; `with_operator_scope` uses `Binding.copy`. **Highest risk** —
   isolated so a green `test/syntax/test_syntax.exe` proves the integration.
2. Repoint `core_loader.ml` syntax cache to `operator_info list`; delete
   `operator_env.ml`; compiler-forced rename of `Operator_env.*` → `Binding.*` in
   `test/syntax/test_enforest.ml`.
3. Confirm `<-` is a genuine base binding lowering to `Syntax.RefSet`, not a
   fallback special-case; verify ref/assignment tests.

## Risks

- **Enforester integration (step 1)** — precedence, newest-wins shadowing, builtin
  fallback ordering, and do-block/module operator scoping via `Binding.copy`. A
  failure in `test/syntax/test_enforest.ml` behavior tests signals a real
  regression here, not an expected test update.

## Resolution

Implemented as planned (Step 0 → Step 3, each built green):

- `operator_info` + an optional `operator` attribute on `Binding.binding_info`
  (orthogonal to `Value`/`Macro` kind); builder helpers `template_infix` /
  `template_prefix` / `macro_infix`; string-keyed newest-wins `find_operator`
  (the `Macro` expansion constructor was renamed `MacroOp` to avoid clashing with
  the `binding_kind` `Macro` constructor in the same module).
- The enforester (`enforest.ml`, `enforest_util.ml`) stores operators in a
  `Binding.t` seeded by `base_operators ()` — all builtins incl. `<-` as
  `BuiltinRefSet`; the old `infix_table`/`prefix_table` fallback is gone.
  `with_operator_scope` now `Binding.copy`s (the table is a mutable Hashtbl).
- `core_loader.ml` syntax cache repointed to `Binding.operator_info list`;
  `operator_env.ml` deleted; the one compiler-forced test rename applied in
  `test_enforest.ml`.

**Deferred to the parent's interleaving work (out of scope here):** a single live
table shared across the parse and expand phases, and operator hygiene
(scope-set-keyed resolution) — there are no scope sets during the parse phase to
key against. Base-operator precedence is still hardcoded in `base_operators ()`;
demoting `+ - * / % < > == !=` / `not` into the prelude is the parent ticket's
step 4.

Verified: `dune build` clean; **787 tests green** (7 + 133 + 312 + 335), including
the operator-heavy backend eval suite, ref-assignment (`<-` → `RefSet`), and
`test_line_counts`.
