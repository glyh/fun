---
title: Elaborator and evaluator agree on binding-list env width only by parallel arithmetic
parent: ../fun-design-map.md
labels:
  - wayfinder:task
  - severity:soundness
status: open
assignee:
blocked_by:
---

# Elaborator and evaluator agree on binding-list env width only by parallel arithmetic

## Question

For every `struct_binding_term`, the elaborator and the evaluator must extend
their environments by the **same number of entries in the same order**, or the
de Bruijn indices of every later binding are wrong. Nothing in the codebase
names that invariant, states it, or checks it — it exists twice, as arithmetic
written independently in two libraries.

## Evidence

| binding | `Elab_infer` pushes | `Nbe` pushes |
|---|---|---|
| `LetBind` | 1 (`Ctx.define`) | 1 |
| `TypeBind` (n params) | n params + ctors + nominal | n `VUnit` pads + ctors + nominal |
| `ImplBind` | 1 (`define_anonymous`) | 1 |
| `OpenBind` | one per public field + public impl (`open_module_value`) | same, `push_opened_values` |

`lib/semantic/typecheck/elab_infer.ml` and `lib/backend/interp/nbe.ml` are in
different libraries (`core_tt_typecheck`, `core_tt_interp`) and share no
definition of this. The `TypeBind` arithmetic is additionally written twice
inside `elab_infer.ml` itself — once in `elab_module_binding`, once in the
`Struct` fold.

## Why it matters now

Adding `OpenBind` required deriving the rule by hand and mirroring it. Getting
it wrong does not produce a type error — it produces `Failure "nth"`, or worse,
a silently wrong value. The related traversal bug
([core traversals ignore binder depth](core-traversals-ignore-binding-list-depth.md))
is the same invariant broken in a third place.

This is the single most likely thing to be mis-transcribed in the planned
CLR/C# rewrite, where the two sides get re-written by different passes of work
and the compiler cannot catch a count mismatch.

## Sketch of the work

1. Give the invariant one home: a function, over `struct_binding_term`, that
   returns the entries a binding contributes. Both sides call it — the
   elaborator to extend `Ctx`, the evaluator to extend `env`.
2. Failing that (the two sides carry different payloads — types vs values), at
   minimum have both derive their count from one shared `binding_width`
   function, and assert the widths agree in a debug build.
3. Fold the duplicate `TypeBind` logic in `elab_infer.ml` into one helper.

## Resolution

_Unresolved._
