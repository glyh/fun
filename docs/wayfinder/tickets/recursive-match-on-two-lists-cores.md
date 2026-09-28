---
title: A deferred arity-2 recursive call cores the compiler on quoted impl evidence
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-28
resolution: Closed 2026-09-28 (`b6ca5ff`, merged as `fork/deferred-call-spine`). The crash is gone: the smallest reproducer went from `exit=134` (SIGABRT, a 22,619-line `Quote → QuoteStuckMatch → OpenArm → Eval` chain) to `VALUE 1`, and the original two-list form answers `VALUE 108` (equal) / `VALUE 109` (unequal) with the impl opened twice — the same as the single-`open` controls. The fix is the design this ticket's recon pointed at: `Value.VGlued` carries the whole argument **spine**, so a deferred call applied a *second* time stays deferred instead of unfolding — supplying an argument is not an inspection — it extends under exactly the conditions it was glued under, `Unfold`/`UnfoldCall` apply the spine (first argument charged, the rest ordinary applications, the `Kont.ApplyArg` frames the evaluator already used), `Quote`/`NeedsShape` fold the spine back as one call, and `SameDeferredCall` compares spines pairwise. **No ceiling, no refusal, no native recursion added, and no `Kont` frame removed.** Suite `907` → **`908` cases, 0 failed**; xUnit `206`; case `imports/eq-list-recursive-helper-open-twice` with its own unit. This **unblocks [Eq for List and Option](std-eq-for-list-and-option.md)**, the two conditional impls the std library surface had to park.
assignee:
blocked_by:
---

# A deferred arity-2 recursive call cores the compiler on quoted impl evidence

Found 2026-09-27 by the [library surface](design-std-library-surface.md) fork and
reproduced by the integrator. Any structural comparison of two lists has this shape, so
[Eq for List and Option](std-eq-for-list-and-option.md) cannot be written without it.

> **The title and premise were wrong, and the reconnaissance below corrected them.**
> It is not two lists: the trigger is a recursive call **applied more than once**
> (arity ≥ 2) whose fixpoint is deferred under the checker, plus a **second**
> ingredient — the impl's evidence must be quoted, which today happens only on the
> duplicate check a **second `open`** of the same unit reaches. The original two-list
> reproducer is one instance, not the rule: arity-2 over *one* list cores too, arity-1
> does not, a non-recursive two-list match does not, and one `open` answers
> `VALUE True`. Reproduced by the integrator: `exit=134` (SIGABRT), with the stack
> running `OpenImpl` → `Convertible` → `QuoteStruct` → `QuoteEntry` → `Quote` →
> `QuoteStuckMatch`, and the single-`open` control `VALUE 1`. The previous fork's
> "cyclic closure environment" attribution is **refuted** — the chain is infinite and
> acyclic, each level a distinct `VVar` at a rising level.

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

## Reconnaissance (2026-09-27, base `f177993`) — root cause pinned

**Mechanism: unbounded (acyclic) native recursion in `Nbe.QuoteStuckMatch`; the
fork's "cyclic closure environment" reading is refuted.** Logging added to
`QuoteStuckMatch` (`Nbe.StuckMatch.cs`, reverted before this commit) at each entry:

```
[qsm] depth=200  width=457  envCount=88 bodies=2 scrut=VVar(lvl=456,spine=0)
[qsm] depth=400  width=725  envCount=38 bodies=2 scrut=VNeutral(HPrim,frames=2)
[qsm] depth=2400 width=3393 envCount=38 bodies=2 scrut=VNeutral(HPrim,frames=2)
[qsm] depth=2800 width=3925 envCount=88 bodies=2 scrut=VVar(lvl=3922,spine=0)
```

Every level's scrutinee is a *distinct* `VVar` at a higher level (`456 → 722 → … →
3922`) and `width` grows with it (`457 → 3925`). A cycle would repeat a value or a
level; this is an infinite acyclic chain, so there is no cyclic environment to read
back. The chain is produced by readback *running the program*: `Quote` hits a neutral
with `Frame.FMatch` → `QuoteStuckMatch`, whose `OpenArm` **`Eval`s each arm body**; for a
recursive helper the arm body is `probe_go(t, …)` with `t` the fresh binder `OpenArm`
just pushed, the call is stuck, and `Eval` returns another neutral+`FMatch` one level
deeper and one width wider. `Quote` recurses natively per level (its comment assumes a
value's structure "never a program's call width"; here the structure *is* the call
width) and the CLR stack dies at ~2900 levels.

**Why the recursive call unfolds at check time: `VGlued` holds one argument.** Under
the checker a known-pure fixpoint call is deferred (`Nbe.Rec.cs:39`,
`Value.VGlued(Fix, Arg, Lazy)`), but applying *that deferred call* to its next argument
is not treated as construction — `Enter`'s `case Value.VGlued glued` (`Nbe.Rec.cs:51`)
unfolds it at once and runs the body. Instrumenting `Enter`:

```
[enter] DEFER probe_go applied to VVar (checking=True)                            ×2933
[enter] FOLD-THROUGH probe_go: second application on a glued call forces unfold   ×2932
```

The DEFER/FOLD-THROUGH pair *is* the crash loop; both counts match the
`QuoteStuckMatch` depth. This contradicts the settled design
([recursive-definitions-stuck-on-open-arguments](recursive-definitions-stuck-on-open-arguments.md)):
a deferred call unfolds "only when something inspects it (force)", and supplying the
second argument is not an inspection.

**The boundary is the recursive call's arity, not two lists.**

| helper under a `pub impl` | result |
| --- | --- |
| ticket's two-list, three-match `probe_go` | core dump |
| arity-2 over **one** list (`probe_go(t, acc + 1)`) | core dump, same `[qsm] depth=2800` |
| arity-1 over one list (`probe_go(t)`) | `VALUE True`; `DEFER×2, FOLD-THROUGH×0` |
| ticket's non-recursive `probe_pair` | `VALUE 2`; never reaches `QuoteStuckMatch` |

The trigger is a recursive call **applied more than once** (directly, arity ≥ 2) whose
fixpoint is deferred under the checker, in a body that readback quotes. A second
ingredient: the impl must be **opened twice** so `OpenImpl`'s duplicate check reaches
`Convertible` (`src/Fun.Compiler/Elaborator.Traits.cs:559-567`), the only caller that
quotes impl evidence here. One `open` gives `VALUE True`; `open` the unit twice aborts.

**Smallest reproducer** (self-contained, no `std` edit; `--file` picks up the sibling
unit):

`probe.unit-mylists.fun`
```fun
open (import "std");
pub rec probe_go = fn(xs : List(I64), acc : I64) : Bool {
  match (xs) { Nil => True, Cons(_, t) => probe_go(t, acc + 1) }
};
pub impl probe : Eq(List(I64)) = module { fn eq(xs, ys) { probe_go(xs, 0) } };
```
`probe.fun`
```fun
{ open (import "mylists"); open (import "mylists"); 1 }
```

It aborts in ~1 s with SIGABRT (exit 134) and prints no line — not even `HANG`. **The
runner's hang detector does not fire**: `StackOverflowException` is uncatchable and
kills the process, and the 60 s elaboration timebox never elapses. So this is worse than
the "looks like a hang" the ticket feared: the whole runner dies, every other case with
it. The budget does not catch it either — readback spends ~one fixpoint step per level,
so ~2900 steps run before the 1 MB stack does, well under `Budget.DefaultLimit`.

**Where the fix goes** (candidate shapes, not chosen):

- **A. Give the deferred call a spine — match the decided design.** `Value.VGlued`
  (`src/Fun.Kernel/Core.Rec.cs:46`) keeps one `Arg` and `Nbe.Rec.cs:51` unfolds on the
  second. Carry the remaining arguments, extend the spine in the `VGlued` case, and fold
  the whole spine back to `Term.Ap(...)` in `Quote` (`Nbe.cs:550`) and `Unify.cs:171,202`;
  unfold only through `Force`/`NeedsShape` (`Nbe.cs:207`). `SameDeferredCall`
  (`Unify.Rec.cs:13`) must then compare spines. Implies: 2+-argument pure calls read back
  as calls exactly as 1-argument ones do, and the lazy-delta shortcut still applies.
- **B. Stop `QuoteStuckMatch` from re-running arm bodies.** `OpenArm`
  (`Nbe.StuckMatch.cs`) evaluates each body to read it back; quote the arm's `Term` under
  the frame's binder width instead, or refuse to re-enter a fixpoint already being
  opened. Implies: a broader change to what a stuck match reads back as, and a risk of
  terms that do not round-trip.
- **C. Budget readback (safety net only).** Make `OpenArm`'s `Eval` spend so divergence
  is a budget error rather than a stack overflow. Implies: a legitimate `open`-twice
  reports "evaluation exceeded the budget" — a wrong rejection, not a fix.

**Not done:** no `src/`, `std/` or `test/` change survives (all instrumentation reverted;
baseline re-measured at `conformance: 865 cases, 0 failed`, xUnit 188/188); the ticket's
exact two-list reproducer was not re-run past the point where the smaller one settles it;
the `Eq(List(A))`/`Eq(Option(A))` library work this blocks was not attempted.
