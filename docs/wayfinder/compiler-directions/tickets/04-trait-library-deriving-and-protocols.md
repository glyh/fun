---
status: open
label: wayfinder:grilling
blockers: []
blocks: []
---

# Trait library deriving and protocols

## Question

Decide the next library-level deriving/protocol operations for traits, using
type-case where possible rather than compiler magic.

## Context

- `ROADMAP.md` records that trait work is mostly complete.
- Remaining milestones include explicit deriving/fallback behaviour and more
  protocol-style operations (e.g. `Eq`, `Ord`, `Show`, `Hash`).
- Where possible these should be implemented as library-level macros or
  type-case generic programming, not additional compiler machinery.

## Resolution

_Unresolved._
