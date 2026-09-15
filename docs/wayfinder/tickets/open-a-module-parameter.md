---
title: `open` a module parameter — names from its signature
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
decided: 2026-09-15
assignee:
blocked_by:
---

# `open` a module parameter — names from its signature

## Today

`open M` needs a module *value*: opening a parameter (a neutral value, only its
type known) is `NotAModule`, in the elaborator and in the evaluator. Left open by
the effects-collect-open-width run, which made `open_module_value` the one path.

```fun
Sig = sig { x : I64 };
f = fn(M : Sig) { open M; x + 1 };   // NotAModule today
f(module { pub x = 41 })             // want 42
```

## Decision (grilled 2026-09-15)

Allowed. What an open binds is decided by the module's **type** (its signature's
members — the slot list, per I2), never by its value. For a neutral module the
opened names are member projections (`M.x`) of the parameter, evaluated when the
function runs. An open of a known module value behaves exactly as before.
