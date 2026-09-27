---
title: Type-case refinement walks the whole context per branch
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Type-case refinement walks the whole context per branch

## Observation (M9 performance fix, 2026-09-14)

`Elab_refine.refine_context_type_var` substitutes a type variable through the
entire elaboration context for each type-case branch. Timed with counters in a
throwaway `init_ctx` benchmark it was 86% of `init_ctx` before M9 (0262a02),
~90% after M9's larger prelude. Commit 46c4a1d made unchanged values return
physically shared and memoised shared values / env tails, taking `init_ctx`
from 26 ms back to 15.8 ms — but the walk is still once per branch over the
whole context, so cost grows with context size × type-case branches.

## Question

Can refinement be scoped to the entries that mention the refined variable
(an index from level to dependent entries), or represented lazily (a
substitution applied on lookup) instead of rebuilding the context? Measure on
`init_ctx` and `test_elaborate.exe` (~8.3 s) before and after.

## Port note (integrator, 2026-09-27) — not forkable as written

**The apparatus this ticket names does not exist in the port.**
`Elab_refine.refine_context_type_var`, the `init_ctx` benchmark and `test_elaborate.exe` are the
deleted prototype's; there is no benchmark project in the port at all (`test/` holds
`conformance`, `Fun.Conformance`, `Fun.Tests`). So the question has to be re-derived against
`src/` and a measurement built before anyone can answer it — the state
[enforester improvements](scope-enforester-improvements.md) was left in, for the same reason.

Two things are already known from reading the port, and one of them moves the question:

- **Half answered.** The port does not walk every value the way the ticket describes.
  `RefineContext` (`src/Fun.Compiler/Elaborator.Patterns.cs:429`) rewrites only the entries whose
  `Level` is at or after the refined variable, with the note *"Only the entries that can mention
  the variable are rewritten, each once per branch - the rule, not the prototype's walk over every
  value."* The prototype's "86 % of `init_ctx`" figure therefore does not transfer as written.
- **What is left of the question** is the ticket's own suggestion, narrowed: an index from a level
  to the entries that *mention* it, so refinement costs the dependents rather than the whole
  suffix. Before building that index, check whether `Substitute` already returns unchanged values
  by physical sharing — the M9 fix (`46c4a1d`) was exactly that trick, and if it survived the port
  the suffix rewrite may already be cheap.

**Not measured.** Nothing was timed in the port — no `init_ctx`-shaped workload exists to time.
"Still expensive" is an assumption carried over from the prototype, not a finding.

## Recon (fork, 2026-09-27) — forkable, re-scoped to a correctness fix

### What is implemented, by name

`RefinementTarget` (`Elaborator.Patterns.cs:449`) finds the matched type variable,
`RefinementOf` (:457) the branch's replacement, `RefineContext` (:472) rebuilds the branch
context, `Substitute` (:483) rewrites one value. `ElaborateMatch` wires them at
`Elaborator.Match.cs:47` (context) and `:48` (expected type); `RefineScrutineeType`
(`Elaborator.Match.cs:79`) is the other half. Refinement *is* observable and works for both
primitive and nominal heads: a later-defined entry `y = x` (type `T`) returned in the `I64`
branch yields `7`, and the same with `Option(I64)`/`Some(n)` yields `5`.

### What is missing: `RefineContext` does not rewrite *every* context entry

It rewrites `Names` and `SelfEntry` only. An entry an `open` introduced lives in
`Opened`, never in `Names` (`OpenModule` → `DefineAnonymous`, `Elaborator.cs:553–561`;
`LocateChoice` reads it at `Elaborator.cs:84`), so refinement misses it:

```
({ M = fn[T : Type](v : T) { module { pub val = v } };
   f : [T : Type] -> T -> I64 = fn[T](x) {
     m = M[T](x);
     open m;
     match (T) { I64 => val, _ => 0 } };
   f(7) })
```

`ELAB type mismatch: cannot unify VVar with VAtomTy(I64)`. Same program without the type-case
returns `7`; reached through a named binding (`y = m.val`) it also returns `7`. So the miss is
specific to `Opened`, and the case belongs in `test/conformance/cases/values/` (expect `7`).

**Fix shape.** Rewrite each `Opened[label][member]` entry by the same `Level` test and
`Substitute`, factoring the per-`Entry` rewrite out of `RefineContext`. `BaseNames` holds
base-context entries only (all levels below any user binder), so it is a no-op today.
`SelfMethods` is a second dictionary of `Value`s not covered at all; a probe could not be built
cleanly because the enclosing struct former rejects a parameter occurring only in method types —
**unsettled**, not ruled out. The performance question (mention index / lazy substitution) is
untouched by this.

### What a replacement benchmark would be — and why none is meaningful

The port has no `init_ctx` analogue: `grep` finds **zero** type-case sites in `std/`, so the
base context is built without a single `RefineContext` call. At program scale refinement is
within noise of the surrounding machinery: a generated D-binding context with a 7-branch
type-case versus the same context without one — D=200: 0.71/0.79 s vs 1.03/1.02 s; D=800:
3.98/3.80 s vs 3.79/3.52 s; the ~30 s at D=2000 is deep-context elaboration, not refinement.
The M9 sharing trick did not survive: `Substitute` unconditionally `Quote`s and `Eval`s.
A replacement measurement would have to be fabricated — D-entry context, deliberately large
entry types, B refining branches, timed via `Driver.Elaborate` in-process — and nothing in the
repo resembles that workload. The performance half should be closed or deferred; only the
correctness fix is forkable.

### Not done

The suite was not run end-to-end (no source touched); probes are single-file `--file` runs.
`SelfMethods` was not settled. No source or test file was edited — this ticket is the diff.
