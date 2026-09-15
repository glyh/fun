---
title: Recursive definitions no longer unfold on open arguments at check time
parent: ../fun-design-map.md
labels:
  - wayfinder:task
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

**Open — decide before closing:** recursion under a stuck branch is a budget
error in principle but not in practice.

```fun
rec fact : I64 -> I64 = fn(n) { if (n == 0) { 1 } else { n * fact(n - 1) } };
F = fn(m : I64) { if (m == 4) { I64 } else { Bool } };
g = fn(n : I64, y : F(fact(n))) { (y : F(fact(n))) }   // conversion unfolds fact under the neutral if
```

Evaluating `fact(n)` stops at the neutral `if`, but converting it with itself
opens both branches and unfolds `fact(n - 1)`, and so on. Each step costs
O(depth) (list-indexed environments under growing binders): 32,768 unfoldings
take ~15 s, so reaching the 1,000,000-call budget takes hours. Options: a much
smaller checker budget; charge by work (e.g. depth) not calls; compare two
identical neutral fixpoint applications without unfolding (syntactic equality
first); or accept.
