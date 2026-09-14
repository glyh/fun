---
title: Recursive definitions no longer unfold on open arguments at check time
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee:
blocked_by:
---

# Recursive definitions no longer unfold on open arguments at check time

## Consequence

The checker budget's "only closed terms evaluate"
([checker-evaluation-budget](checker-evaluation-budget.md)) is implemented as:
a fixpoint unfolds at check time only when its argument is closed; otherwise
the call is a neutral `HFix`. So with `rec double = fn(n) -> n + n`,
`double(n)` is no longer convertible with `n + n` under a binder `n`, where
before it was. Non-recursive lambdas still β-reduce on open arguments.

## Question

Is this the intended reading of the decision? Alternatives:

- **Keep it** (Zig-like): recursive definitions are opaque on open arguments.
  Predictable, never hangs, but loses equations users may rely on.
- **Unfold once on open arguments, stuck on recursion** (Agda/Lean-style
  delta with guarded unfolding): `double(n)` ≡ `n + n`, `loop(n)` stays stuck.
  Needs a rule for what "recursion" means for mutual `rec` groups.
- **Unfold under the budget regardless**: open calls cost budget; a divergent
  open call is a budget error rather than stuck, contradicting the ticket's
  `fn(n : I64, y : loop(n))` example.

## Where

`lib/backend/interp/nbe.ml` (`closed`, `HFix`), `nbe_quote.ml`, `unify.ml`.

## Found by

The checker-budget implementation (2026-09-14).
