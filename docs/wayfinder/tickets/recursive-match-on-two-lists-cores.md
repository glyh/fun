---
title: A recursive helper matching two lists cores the compiler
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by: []
---

# A recursive helper matching two lists cores the compiler

Found 2026-09-27 by the [library surface](design-std-library-surface.md) fork and
reproduced by the integrator. Any structural comparison of two lists has this shape, so
[Eq for List and Option](std-eq-for-list-and-option.md) cannot be written without it.

## The reproducer

A recursive helper at a unit's top level, plus a trivial `pub impl` whose body calls it,
plus `export Lists.{probe};` in `std/stage2.fun` — then build and run a program that
compares two lists with `==`:

```fun
rec probe_go = fn(xs : List(I64), ys : List(I64)) : Bool {
  match (xs) {
    Nil => match (ys) { Nil => True, Cons(_, _) => False },
    Cons(h, t) => match (ys) {
      Nil => False,
      Cons(h2, t2) => match (i64_to_bool(eq_i64(h, h2))) { True => probe_go(t, t2), False => False }
    }
  }
};
pub impl probe : Eq(List(I64)) = module { fn eq(xs, ys) { probe_go(xs, ys) } };
```

Both an equal pair and an unequal pair give the same result:

```
=== equal lists   -> at System.Threading.Thread.StartCallback()
                      timeout: the monitored command dumped core
=== unequal lists -> timeout: the monitored command dumped core
```

## What it is and is not

- **Not the two-list match.** The same nested match without the recursive call runs
  fine: `probe_pair = fn(xs, ys) { match (xs) { Nil => 0, Cons(h, t) => match (ys) {
  Nil => 1, Cons(h2, t2) => 2 } } }` answers `VALUE 2`.
- **Not recursion over a list by itself.** `Lists.length` — a `rec go` that matches one
  list and calls itself — is in the shipped library and every case passes.
- **Not the impl.** The impl only calls the helper; the helper alone is the trigger.
- So the trigger is **a recursive helper that matches on two lists**.

The fork attributes the crash to `Nbe.QuoteStuckMatch`, reading back a closure
environment that is cyclic. That is the fork's reading, not a measurement: the crash is
reproduced here, the mechanism was not.

## Why it matters

`Eq(List(A))` and `Eq(Option(A))` are structurally recursive over two values, and so is
every `zip`-like or `starts_with`-like function a user will write once the library gives
them the type. Until this is fixed, the shipped library can offer
[no equality for lists](std-eq-for-list-and-option.md) even after
[the bound gap](generic-impl-head-var-has-no-bound.md) closes.

A crash is worse than an error here: the process goes down with no `ELAB`/`VALUE` line,
so a conformance case cannot even record the failure — a case that hits it looks like a
runner hang.

## Reading

- `src/Fun.Compiler/Nbe.Match.cs`, `src/Fun.Compiler/Nbe.StuckMatch.cs` — `QuoteStuckMatch`
  and the environment read-back the fork named
- `src/Fun.Compiler/MatchCompile.cs` — the decision trees the two-argument match compiles to
- `test/conformance/cases/values/` — where a case belongs once it passes; the runner's
  hang detector (`port-runner-does-not-timebox-elaboration`) is what a reproducer meets
  first
