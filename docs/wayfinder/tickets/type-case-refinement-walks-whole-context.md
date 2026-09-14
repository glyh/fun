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
