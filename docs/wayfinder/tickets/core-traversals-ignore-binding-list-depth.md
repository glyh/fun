---
title: Core term traversals ignore binder depth in binding lists
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Core term traversals ignore binder depth in binding lists

## Question

Every `Core.term` traversal that carries a de Bruijn cutoff/depth walks a
`Module`/`Struct` binding list with that number held **constant**, even though
each binding extends the environment. Is that a latent bug, and does the new
`OpenBind` make it unfixable as written?

## Evidence

`Nbe`'s evaluator pushes one env entry per `LetBind`/`EffectBind`/`ImplBind`/
`PatternSynBind`, several per `TypeBind`, and N per `OpenBind`. But:

- `Elab_defs.shift_term` — `Module { bindings }` maps `shift cutoff` over every
  binding with the same `cutoff`.
- `Elab_refine.close_recursive_payload_term` — same, with `go cutoff`.
- `Elab_generalize.closed_under` — same, with `depth`.

So a `Var` in the *k*-th binding is treated as if no earlier binding had bound
anything. Today these traversals are only reached from constructor-payload
terms, which is why nothing fails; the wrongness is latent, not observed.

## Why `OpenBind` sharpens it

For `LetBind` the fix is mechanical (advance by one; `TypeBind` by its known
count). For `OpenBind` the count is **not recoverable from the term** — it is
the number of public fields of a module the traversal would have to evaluate.
So either these traversals must never see a binding list containing an open, or
`OpenBind` has to carry the width it contributes.

## Sketch of the work

1. Confirm whether any of the three traversals can actually reach a module or
   struct binding list. If none can, replace the per-binding maps with an
   explicit `assert false`/`failwith` (or drop the cases) rather than leaving
   quietly-wrong code that reads as if it handles them.
2. If they can, thread the depth through the fold and decide how `OpenBind`
   reports its width — most likely by storing the opened field count on the
   term at elaboration time, where it is already known.

## Resolution

_Unresolved._
