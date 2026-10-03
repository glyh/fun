---
title: Record the absence of subtyping
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee:
blocked_by: []
---

# Record the absence of subtyping

## Question

Confirm and record that `fun` has no subtyping relation — convertibility (NbE) is the
only equality, records are structural, and no subtype rule exists anywhere.

## Context

- The Graydon-constraint audit (2026-10-01,
  [review-2026-10-01.md](../../ideas/review-2026-10-01.md)) found **zero** mentions of
  subtyping across `docs/wayfinder/fun-design-map.md`, `docs/STATUS.md`, and
  `docs/wayfinder/topics/`. It is the one row of Graydon's ten constraints with no record
  at all.
- The de facto answer is "none": there is no subtype relation, records are structural,
  and the only equality is NbE convertibility. But no document says so.
- The corpus's rival approach (a bounded set of subtyping rules — a mutable reference a
  subtype of a shared one) is roughly what heap brands do *without* a subtyping rule:
  `Ref(h, A)` and `Ref(h', A)` are simply different types, related by nothing.

## Resolution

_Unresolved._
