---
title: Recursive definitions no longer unfold on open arguments at check time
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-15
resolution: Recursive definitions unfold on open arguments under the checker budget, which measures work; two calls of one known-pure fixpoint on convertible arguments convert without unfolding (lazy delta).
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

## Grilled (2026-09-15): unfold under the budget

**A recursive definition unfolds on open arguments like any other call, under
the checker's evaluation budget; exhausting the budget is an error.** Same
budget and same error as the rest of type checking (and macro expansion, M5).

```fun
rec double = fn(n) { n + n };
f : (n : I64) -> Vec(double(n)) -> Vec(n + n)   // ok: double(n) unfolds to n + n
g : (n : I64) -> Vec(fact(n)) -> I64            // error: evaluation budget exceeded, calling fact
```

- No "stuck on recursion" rule, so no rule for what counts as recursion in a
  mutual `rec … and …` group.
- A type mentioning a recursive call on an open argument that does not reduce
  (`fact(n)`, `loop(n)`) is a budget error, not a stuck term. This reverses the
  closed [checker-evaluation-budget](checker-evaluation-budget.md) ticket's
  `fn(n : I64, y : loop(n))` example (stuck, via `HFix`).
- Considered and rejected: unfold once (needs the mutual-recursion rule);
  budget exhaustion leaves the call stuck (type equality would depend on the
  budget size).

## Implemented (2026-09-15), one question open

- `HFix`, `Nbe.closed`/`closure_closed`/`closure_slots` and
  `Eval_budget.checking` are deleted: a fixpoint unfolds on any argument.
- A unification is now one budget request (`Ctx.unify`/`try_unify`); before,
  each closure evaluation inside it refilled the budget, so a divergent
  conversion never ran out.
- Tests: `double(n)` converts with `n + n`; a call through another recursive
  definition unfolds; `loop(n)` in a parameter type and a closure capturing an
  unknown variable are budget errors. (There is no value-level `rec … and …`,
  so no mutual group rule is needed.)

**Decided and implemented (2026-09-15): lazy delta guarded by purity, and a
work budget.**

1. **Two calls of the same fixpoint on convertible arguments are equal without
   unfolding** — only when the fixpoint is known pure. Under the checker a
   pure fixpoint's call is a deferred value (`VGlued`), unfolded only when
   something inspects it (`force`); conversion and unification compare two
   deferred calls of the same closure by their arguments first and fall back
   to unfolding. A deferred call quotes as the call.
   - **Purity criterion:** the fixpoint's type is an arrow whose effect row is
     closed and empty, as its annotation reads after checking
     (`Ctx.pure_call`, stored in `Fix`/`VFix`). An open row (today every bare
     arrow, until [bare-arrow-is-pure](bare-arrow-is-pure.md)) is not known
     pure, and an effectful row is not pure: those calls unfold eagerly.
2. **The checker budget measures work:** every call and every step of a
   conversion or unification spends. Converting calls of two different
   fixpoints with the same body (`fact(n)` vs `fact2(n)`) is a budget error in
   well under a second.

```fun
rec fact : I64 -> I64 can {} = fn(n) { if (n == 0) { 1 } else { n * fact(n - 1) } };
g = fn(n : I64, y : F(fact(n))) { (y : F(fact(n))) }   // ok, no unfolding
```

`fact(n)` vs `fact(m)` is a type mismatch, not a budget error: after one unfold
the stuck scrutinees `n == 0` and `m == 0` differ.
