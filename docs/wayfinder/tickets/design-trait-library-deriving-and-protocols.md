---
title: Trait library deriving and protocols
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee:
blocked_by: []
---

# Trait library deriving and protocols

## Question

Decide the next library-level deriving/protocol operations for traits, using
type-case where possible rather than compiler magic.

## Context

- The [map](../fun-design-map.md) and [`docs/STATUS.md`](../../STATUS.md) record trait work
  as mostly complete; detail in [traits](../topics/traits.md) and
  [trait-module-stdlib](../topics/trait-module-stdlib.md).
- Remaining milestones include explicit deriving/fallback behaviour and more
  protocol-style operations (e.g. `Eq`, `Ord`, `Show`, `Hash`).
- Where possible these should be implemented as library-level macros or
  type-case generic programming, not additional compiler machinery.

## Resolution

_Unresolved._
