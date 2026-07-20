---
title: Private type visibility model
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee:
blocked_by: []
---

# Private type visibility model

## Question

Decide the private/opaque type visibility model, and whether it should remain
design-only until a specific implementation trigger exists.

## Context

- Not to be implemented during a concrete bug-fix pass (separate design task).
- [private-type-visibility](../topics/private-type-visibility.md) tracks the OCaml/SML model:
  private types leak through public bindings but are abstract outside the
  defining module — constructors are unavailable unless the type is public
  and concrete.

## Resolution

_Unresolved._
