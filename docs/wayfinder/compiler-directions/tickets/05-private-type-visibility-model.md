---
status: open
label: wayfinder:grilling
blockers: []
blocks: []
---

# Private type visibility model

## Question

Decide the private/opaque type visibility model, and whether it should remain
design-only until a specific implementation trigger exists.

## Context

- `TODO.md` says do not implement during the current concrete bug-fix pass.
- `docs/16.private_type_visibility.md` tracks the OCaml/SML model:
  private types leak through public bindings but are abstract outside the
  defining module — constructors are unavailable unless the type is public
  and concrete.

## Resolution

_Unresolved._
