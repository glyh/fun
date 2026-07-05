---
status: open
label: wayfinder:grilling
blockers:
  - 01-type-aware-macro-interleaving
blocks: []
---

# Enforester improvements scope

## Question

Decide which enforester improvements are worth doing in the OCaml prototype
before the CLR / C# rewrite.

## Context

- `docs/15.enforest_improvement_plan.md` describes two phases:
  1. Structured errors and fault-tolerant parsing (spans, recovery, incremental).
  2. Spec-oriented structure (declarative combinators, generic driver).
- The roadmap cautions against broad diagnostics cleanup pre-rewrite.
- Some enforester work may be necessary to unblock macro feature work, but
  the boundary is not decided.

## Resolution

_Unresolved._
