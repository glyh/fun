---
title: Core term traversals ignore binder depth in binding lists
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
assignee: glyh
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

**Measured, then made loud.** Step 1 asked whether the three traversals can
actually reach a module or struct binding list. They cannot, as far as the test
suite goes.

All four cases were instrumented and the whole suite run: **zero hits**. The
instrumentation was then confirmed to fire by calling `shift_term` directly on a
`Module` term, so zero means never reached rather than a broken probe.

The wrongness is only harmless while a list holds at most one binding, since
nothing then follows the binding that widens the environment. That condition now
has a name, `Elab_defs.binding_list_depth_is_tracked`, and the three traversals
use it:

- `Elab_defs.shift_term` (`Module` and `Struct`) and
  `Elab_refine.close_recursive_payload_term` (`Module`) must return a term, so
  they refuse a longer list via `Elab_defs.reject_untracked_binding_list`.
- `Elab_generalize.closed_under` is a predicate and so has a safe answer instead:
  a term whose binder depths cannot be tracked is not *known* to be closed, so it
  returns `false`. That only declines the generalization it gates, which is
  always sound.

No arithmetic was invented for a case that has never occurred — and the
`OpenBind` obstacle this ticket raised is unchanged, so inventing it would have
meant guessing at a width that is not in the term. If one of these ever fires,
the message names this ticket.

Suite green, 816 tests. The shared contract this depends on now lives in
`Core.binding_width`; see
[env-width-contract-is-unnamed](env-width-contract-is-unnamed.md).
