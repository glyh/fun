---
title: The unhandled-effect run-time error says handlers are not implemented
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# The unhandled-effect run-time error says handlers are not implemented

`Nbe_support.unhandled_effect_error` (`nbe_support.ml:70-76`) reports
`unhandled effect Exc.raise; handlers are not implemented`. Handlers are
implemented (deep, one-shot, `resume` lexically scoped — E7–E9). Drop the
suffix; the message should say only which operation had no handler (and, once
elaborator errors carry sites, where it was performed).

Probed on main (35baa3c): `{ effect Exc = sig { raise : I64 -> I64 }; perform Exc.raise(1) }`.

Found by the effects domain-model audit (2026-09-15).
