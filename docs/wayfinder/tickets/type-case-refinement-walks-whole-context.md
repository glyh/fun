---
title: Type-case refinement walks the whole context per branch
parent: ../fun-design-map.md
labels:
  - wayfinder:research
status: open
assignee:
blocked_by:
---

# Type-case refinement walks the whole context per branch

## Observation (M9 performance fix, 2026-09-14)

`Elab_refine.refine_context_type_var` substitutes a type variable through the
entire elaboration context for each type-case branch. Timed with counters in a
throwaway `init_ctx` benchmark it was 86% of `init_ctx` before M9 (0262a02),
~90% after M9's larger prelude. Commit 46c4a1d made unchanged values return
physically shared and memoised shared values / env tails, taking `init_ctx`
from 26 ms back to 15.8 ms — but the walk is still once per branch over the
whole context, so cost grows with context size × type-case branches.

## Question

Can refinement be scoped to the entries that mention the refined variable
(an index from level to dependent entries), or represented lazily (a
substitution applied on lookup) instead of rebuilding the context? Measure on
`init_ctx` and `test_elaborate.exe` (~8.3 s) before and after.

## Port note (integrator, 2026-09-27) — not forkable as written

**The apparatus this ticket names does not exist in the port.**
`Elab_refine.refine_context_type_var`, the `init_ctx` benchmark and `test_elaborate.exe` are the
deleted prototype's; there is no benchmark project in the port at all (`test/` holds
`conformance`, `Fun.Conformance`, `Fun.Tests`). So the question has to be re-derived against
`src/` and a measurement built before anyone can answer it — the state
[enforester improvements](scope-enforester-improvements.md) was left in, for the same reason.

Two things are already known from reading the port, and one of them moves the question:

- **Half answered.** The port does not walk every value the way the ticket describes.
  `RefineContext` (`src/Fun.Compiler/Elaborator.Patterns.cs:429`) rewrites only the entries whose
  `Level` is at or after the refined variable, with the note *"Only the entries that can mention
  the variable are rewritten, each once per branch - the rule, not the prototype's walk over every
  value."* The prototype's "86 % of `init_ctx`" figure therefore does not transfer as written.
- **What is left of the question** is the ticket's own suggestion, narrowed: an index from a level
  to the entries that *mention* it, so refinement costs the dependents rather than the whole
  suffix. Before building that index, check whether `Substitute` already returns unchanged values
  by physical sharing — the M9 fix (`46c4a1d`) was exactly that trick, and if it survived the port
  the suffix rewrite may already be cheap.

**Not measured.** Nothing was timed in the port — no `init_ctx`-shaped workload exists to time.
"Still expensive" is an assumption carried over from the prototype, not a finding.
