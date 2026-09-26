---
title: Enforester improvements scope
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee:
blocked_by:
---

# Enforester improvements scope

> **Unblocked 2026-09-26** — `design-type-aware-macro-interleaving.md` closed, so the blocking
> edge was stale. Re-framed the same day: this ticket was written to scope work *in the OCaml
> prototype before the C# rewrite*, and the prototype was deleted 2026-09-25 while the port became
> the implementation. The question survives; its targets do not.

## Question

Which reader / enforester improvements are worth doing **now** — with `src/Fun.Expand` the only
implementation, no rewrite coming to make the effort disposable, and the surface still moving?

## Context

- [enforester-improvements](../topics/enforester-improvements.md) describes two phases:
  1. Structured errors and fault-tolerant parsing (spans, recovery, incremental).
  2. Spec-oriented structure (declarative combinators, generic driver).

  **That topic is written against the deleted prototype** — every path and code block in it is
  `enforest*.ml`. It is an *input* to this question, not an inventory of the port; re-deriving it
  against `src/Fun.Expand/Reader.cs`, `Enforest*.cs` and `Expander*.cs` is part of the answer,
  and nothing measured today backs any of the two phases' claims.
- The old roadmap cautioned against broad diagnostics cleanup pre-rewrite. The rewrite has
  happened, so that constraint is void — but "one implementation, surface still moving" is not
  the same as "polish now". That boundary is what this ticket decides.
- Work that already points at the enforester from elsewhere:
  [Stage 12 diagnostics](specify-stage-12-macro-diagnostics-and-expansion-ux.md) (there is no
  non-fatal diagnostic channel), [the runner does not timebox elaboration](port-runner-does-not-timebox-elaboration.md)
  (a reader that stops advancing must fail the suite, not hang it), and
  [brackets decide grouping](brackets-decide-grouping.md) (open hole-extent questions).

## Resolution

_Unresolved._
