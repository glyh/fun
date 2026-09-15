---
title: Opening a module needs its value, and a non-module open is skipped silently
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-15
assignee:
blocked_by:
---

# Opening a module needs its value, and a non-module open is skipped silently

Found by the domain-model audit (2026-09-15), re-verified on `main` `fa2f32d`.

## Invariant

**I2** (`topics/core-tt-domain-model.md`): elaborator and evaluator widen a
context identically per binding, from the slot list alone.

## Where the code deviates

- `Elab_resolve.open_module_value` (`lib/semantic/typecheck/elab_resolve.ml:236-261`)
  zips the module's *type* entries with its *value* entries, so an open's width
  depends on having the value. Unmatched entry pairs fall through `| _ -> c`
  (line 251) and a non-module falls through `| _ -> ctx` (line 261), both silently.
- Callers in `elab_infer.ml` raise `NotAModule` first, but effect collection
  (`elab_effect_collect.ml:199-207`) collects the body without the open
  (`| _ -> ops.collect_effects ctx body`).

## Direction

Read an open's members from the module *type* (its slot list) and treat the
value only as the payload. Make the mismatch cases an internal-invariant error,
and route effect collection through the same helper so it cannot skip an open.

## Resolution (2026-09-15)

`open_module_value` is the one path for every open (expression, module item,
struct item, effect pass): a non-module is `NotAModule`, the entries come from
the module type (one per public field and impl, the slot list the evaluator
pushes), and a value whose entries do not line up with its type is an internal
invariant failure, not a narrower open. Opening a module *parameter* (a neutral
value) stays `NotAModule`, as the evaluator also requires a module value.
Test: `NotAModule` for `open 5` in an expression, a lambda body and a module.
