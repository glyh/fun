---
title: Elaborator and evaluator agree on binding-list env width only by parallel arithmetic
parent: ../fun-design-map.md
labels:
  - wayfinder:task
  - severity:soundness
status: closed
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

**Partially resolved — step 2 done, the ticket stays open for step 1.**

The invariant now has one home: `Core.binding_width : struct_binding_term -> int
option` in the kernel, which both libraries can see. It returns `None` for
`OpenBind`, whose width is the public-entry count of a module that must be
evaluated first and so is genuinely not recoverable from the term.
`Core.binding_list_width` totals a list, or `None` if it contains an open.

`Nbe` now cross-checks its own env growth against it after both binding folds,
the `Module` one and the `Struct` one. A drift on either side raises an
`EvalError` naming this ticket instead of surfacing later as a `Failure "nth"` or
a silently wrong value.

Verified the check is real rather than decorative: perturbing each branch of
`binding_width` in turn — the flat `Some 1` and the `TypeBind` sum — makes the
suite fail, and restoring it makes it pass. So both branches are exercised by
real programs, and the evaluator's hand-written arithmetic provably agrees with
the shared function across all 816 tests.

### Still open

- **Step 1**, the real fix: have the *elaborator* extend `Ctx` from
  `binding_width` too, rather than only being checked against it. Today the check
  is one-sided — it pins the evaluator to the contract but the elaborator still
  derives its extension independently.
- **Step 3** was mis-scoped as small. The two `TypeBinding` implementations in
  `elab_infer.ml` are not a mechanical duplicate: `elab_module_binding` splits on
  a zero-parameter fast path and the `Struct` fold handles the parameterised case
  generally. Merging them is a real refactor with no behavioural payoff on its
  own, and should ride along with step 1 rather than be done for its own sake.

## Closed

**Both remaining steps done, and the check earned its keep immediately.**

The elaborator now cross-checks its own context growth against
`Core.binding_width` in both binding folds, the `Module` one and the `Struct`
one, and reports a `BindingWidthDrift` naming this ticket. The contract is
pinned from both sides instead of one.

Turning the check on failed at once: a `type` member inside a `struct` pushed one
context entry too many, two when parameterised, so every member written after it
resolved to the wrong de Bruijn index. Nothing in the suite covered a `struct`
with a type member followed by anything else. Two regressions now do.

Step 3 came with it, as the ticket said it should. The two `TypeBinding`
implementations are one `elab_type_binding`, used by both folds, and the drift
was the difference between them: the `Struct` copy named the type once before its
constructors and once after. The shared version names it once, after, which is
the order `Nbe` pushes.

## Closed again, on a different design

The ticket asked, as step 1, that the elaborator *derive* its extension from
`Core.binding_width`. That is not achievable as written: the function returns a
count, and the elaborator pushes entries carrying a name, a type and a value in a
chosen order, alongside scaffolding entries that are deliberately not part of the
width. Deriving would amount to calling the function and asserting on the result,
which is the check that already existed.

What both sides can genuinely share is the **shape**. `Core.binding_slots`
replaces `binding_width` with an ordered list of slots, one per entry a binding
adds, each carrying a name where it has one and where its payload comes from: a
term to evaluate, a value the term already holds, or a stand-in each side fills
in for itself. `OpenBind` still has no list, for the same reason it had no width.
Width survives only as the length of that list.

- **Evaluator:** pushes the slots directly. Its module fold and struct fold were
  the same 55 lines twice, differing only in which entry constructors they built;
  they are now one fold parameterised by those constructors. Its own cross-check
  is deleted, because it would compare the contract with itself.
- **Elaborator:** zips its payloads — a type and a value per slot — onto the same
  list in `elab_type_binding`. A shape disagreement now fails while the context is
  being built rather than as a wrong de Bruijn index later.
- **Impls and traits** extend the context inside their own elaborator and do not
  go through slots, so their contribution is still a second opinion and keeps the
  binding-list check. That is the remaining seam, and it is the one the port
  should close by construction.

Verified load-bearing the same way as before: adding one slot to the nominal case
fails the suite broadly, removing it restores it. 842 tests green.
