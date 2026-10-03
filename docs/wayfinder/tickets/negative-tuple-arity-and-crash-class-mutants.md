---
title: The negative-tuple-arity refusal, and crash-class mutants in the suite
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# The negative-tuple-arity refusal, and crash-class mutants in the suite

> **Filed 2026-10-03** — the mutation sweep row for `Primitives.cs:165` (in
> [coverage-gaps-from-the-mutation-sweep](coverage-gaps-from-the-mutation-sweep.md)) said the
> refusal is "only observable as a process-killing stack overflow". Measured that day: the
> guard is correct semantics, and the suite can now pin it — with a gap that remains.

## The item

`Primitives.cs:165` — `TupleArityType`'s `if (n.Value < 0) throw new FunException("Tuple: the
number of components is negative")`. Removing it (the sweep's mutant) makes `tuple_arity(-5)`
return `Type -> tuple_arity(-6) -> …`, an infinite `VPi` chain, and consumers diverge:

- **Re-refuse:** unifying the chain with `VU` gives `type mismatch: cannot unify VPi with VU` —
  still `error`, so a case expecting `error` still passes, and the mutation is invisible.
- **Force-walk:** `Unify.Mentions` recurses natively over the chain, each level forcing the next
  `tuple_arity(n-1)` through the evaluator. The native stack overflows — a
  `StackOverflowException`, which .NET does not let a catch intercept, so the process dies
  (verified: exit 134, core dump) taking every other case's result with it. The same walk can
  also still be running when the runner's 60s elaboration timeout fires (verified under
  `--isolated`). Which fires first is a race; both were observed.

## Ruling

**The guard stays, and it is the assertion.** `Tuple(n, …)`'s first argument must be positive;
`Primitives.cs:165` asserts it. No depth budget is added to the evaluator — the refusal is the
correct semantics, not a case a budget should absorb.

## What pins it now

- `elaborate/tuple-negative-arity.fun` (`{ y = Tuple(0 - 5); 1 }`, expects `error`) — passes on
  the clean tree, and **fails on the mutant** (observed under `--isolated`: the 60s elaboration
  timeout). The guard is pinned in-process, at the cost of a minute when it is broken.
- The runner's `--isolated` mode (filed 2026-10-03, `test/Fun.Conformance/Program.cs`) runs
  each case as a `--case` child process, so a crash is one case's failure. Verified against
  the mutant: the runner survives, reports the case as failed, exits 1.

## Remains

- **The in-process suite cannot be trusted with this mutant.** The case above is in the suite,
  and on the mutant its elaboration never terminates — it either overflows (runner dies) or
  times out at 60s (survivable), a race. `--isolated` removes the race: both outcomes are one
  case's failure.
- **The crash-class shape has no in-process case.** `{ g = fn(x) { x }; g(Tuple(0 - 5)); 1 }`
  stack-overflows the mutant in `Unify.Mentions` (verified: exit 134); no case in
  `test/conformance/cases` covers that spelling, so the in-process suite cannot catch a
  regression that turns the refusal into a crash.
- **The mutation sweep should run with `--isolated`.** Any mutant whose failure mode is a crash —
  this one's shape, or any future infinite-type bug — is only classifiable per-case in isolated
  mode.
