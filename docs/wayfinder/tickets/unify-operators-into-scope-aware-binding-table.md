---
title: Unify operators into the scope-aware binding table
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
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

## Risks / the real work

- **Enforester-env ↔ binding-table unification is the hard part.** Today the
  enforester's `env.operators` (`lib/expand/enforest_util.ml`) is a *different
  structure* from `Expand_ctx.binding_table`. The precedence parser and the macro
  resolver must be made to consult one table. That integration — not the operator
  move itself — is the bulk of the work.
- **Operator hygiene needs scope-sets on operator tokens.** `+` / `&&` lex as
  `Operator "…"`; to resolve them hygienically (a locally-shadowed `+` resolving
  by scope set, like macros do now) they must participate in scope sets like
  identifiers do.

## Resolution

_Unresolved._
