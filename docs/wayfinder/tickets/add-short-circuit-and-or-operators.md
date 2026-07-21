---
title: Add short-circuit && / || operators
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Add short-circuit && / || operators

## Question

Add `&&` and `||` as short-circuit boolean operators over the library `Bool` ADT.

## Context

- Graduated from [Stage 11 macro-powered language features spec](specify-stage-11-macro-powered-language-features.md).
- They must be **macros/templates, not functions** — the RHS must not be evaluated
  eagerly. Expand `a && b` ⇒ `match a do True -> b | False -> False end` and
  `a || b` ⇒ `match a do True -> True | False -> b end`.
- Feasible **now** as builtin operators (same status as `<-`): add entries to
  `operator_env.ml`'s `infix_table` with a `Template`/`Macro` expansion, and add a
  lexer path for `&`/`|` (`raw_syntax.ml:92` — `|` is currently a bare `Bar`).
- Does **not** depend on the prelude-operator-propagation mechanism.

## Resolution

_Unresolved._
