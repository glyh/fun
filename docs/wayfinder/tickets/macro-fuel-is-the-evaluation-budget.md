---
title: Macro fuel is the evaluation budget; expansion failures are error values
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Macro fuel is the evaluation budget; expansion failures are error values

## Decision (macro model M5, M8)

A macro application is a call, and it counts under the checker's one
evaluation budget. The separate application-nesting fuel
(`Expand_ctx.macro_fuel`, reserve/release, 256) is retired: it misses breadth
blowup, where each output spawns sibling calls at bounded depth. Exhausting
the budget is an error value naming the call.

The same change retires the `failwith`s at application sites, which are
exception-as-control-flow per `CLAUDE.md`: a kind mismatch ("macro 'm' has
kind Decl but was used in Expr context"), a non-syntax result on the untyped
and operator paths, and a missing callback.

## Where

- `Expand_ctx.with_macro_fuel` / `reserve_macro_fuel` and every caller in
  `expand.ml` and `elab_resolve.run_type_aware_macro` (through
  `macro_runtime.with_fuel`).
- `run_macro_call` and the decl/operator paths in `expand.ml` for the
  `failwith`s.

Blocked on [checker-evaluation-budget](checker-evaluation-budget.md), which
introduces the budget this counts against.
