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

## Oracle second opinion (2026-09-28) — recon confirmed but too narrow; forkable now

An oracle run (research only; real repo untouched, experiment at `/tmp/orc/repo`, probes at
`/tmp/orc/*.fun`) verified the recon against the code and the runner.

**Q1 confirmed, line numbers corrected.** The reproducer fails as reported; both controls return
7. `RefineContext` is at `Elaborator.Patterns.cs:567`, `RefinementTarget` at `:545`, `Substitute`
at `:578` — the recon's numbers had drifted.

**Q2: `Opened` is one of five missed channels, not the only one.** Each probed, failing on `main`,
passing once rewritten; all five together keep the suite at **947 cases, 0 failed**:

| Channel | Probe | On `main` | Rewritten |
|---|---|---|---|
| `Opened` (the recon's case) | `a.fun` | `VVar` vs `I64` | 7 |
| `ConstructorEntries` (`open Opt(T)`, then `Some2(x)` / a `Some2(n)` pattern) | `ce1`, `ce2` | mismatch | 1, 8 |
| `ResumeEntry` (`resume(1) + 1` in a refined branch) | `rs.fun` | mismatch | 8 |
| `SelfMethods` | `sm.fun` | mismatch | 8 |
| `Evidence` (bound `[A : Size]` used in the `Char` branch, impl after `f`) | `ev4.fun` | `missing implementation of Size` | 9 |

- **`SelfMethods` is settled, not unsettled.** The recon's probe failed only because its type
  parameter appeared solely in method types; a field `v : T` routes it through `SelfEntry`.
- **`ResumeEntry` is a port regression**: the prototype's `refine_context_type_var` rewrote
  `resume_entry` (`git show 46c4a1d:lib/semantic/typecheck/elab_refine.ml`, ~line 153); the port
  dropped it.
- **`BaseNames` needs no change, but the recon's reason is wrong.** Base levels are *not* always
  below the refined variable — `match (I64)` refines level 0. It is a no-op because base entry
  types are closed.
- **`Opened` and `ConstructorEntries` were rewritten together**, so which needs which is not
  isolated; probably both (patterns resolve through `ConstructorEntries`, `Elaborator.Enum.cs:230`).

**Q3: the recon measured the cheap half.** It refined a bound `T`; the expensive case is a
matched name *with a value* (a builtin, or `U = T`), which rewrites every later entry:

| Program | Refined | Not refined (`match (id(T))`) |
|---|---|---|
| 200 entries × 50 `match (T)` | 2.46 s | 2.42 s |
| 200 × 50 `match (I64) { I64, Char, String, Unit, _ }` | **37.8 s** | 0.67 s |
| 100 × `match (I64)` inside a generic function | 4.0 s | 0.8 s |

The fix and the alias hole are one change: in `RefinementTarget`, refine only when the scrutinee
forces to a bare variable — `ctx.Force(ctx.Eval(scrutinee)) is Value.VVar { Spine.Length: 0 } v ?
v.Level : null`. That takes 37.8 s → 0.67 s and 4.0 s → 0.85 s with the suite green. The M9
sharing trick indeed did not survive (`Substitute` always `Quote`s then `Eval`s), and a mention
index is not justified by any measured number — **defer it; do not close the performance half on
the recon's evidence.**

**New hole the recon missed — aliases.** Only the matched variable is refined, never the one it
stands for: `U = T; match (U) { I64 => x + 1 }` fails (`al.fun`), and passes with the bare-variable
target above (suite still green).

**One ruling owed to the user, not to a fork.** `d.fun` (`y : T` written in the branch) and
`e.fun` (`y : U`): *should a type written inside a branch see the matched variable as the matched
head?* Replacing `T`'s value slot fixes `d` but breaks `core-072`–`077` ("a meta's spine must be
distinct variables"); `e` fails either way.
`docs/wayfinder/topics/type-case-generic-programming.md:88` says the checker "may treat the matched
type variable as equal" — permission, not a decision. Take it to grilling; keep it out of the fix.

**Fork brief (recommended order):**
1. Factor one per-entry rewrite; apply it to `Names`, `SelfEntry`, `ResumeEntry`, every `Opened`
   member; `Substitute` over `SelfMethods` values, both `ConstructorEntries` types, and each
   `Evidence` entry's arguments and type.
2. Bare-variable target in `RefinementTarget` (fixes aliases + the 37.8 s case).
3. One case per probe (`a`, `ce1`, `ce2`, `rs`, `sm`, `ev4`, `al`) in
   `test/conformance/cases/values/`, each proven to fail on `main` first.
4. `d`/`e` go to a design ruling, not into this fix.

Verify: `dotnet run --project test/Fun.Conformance` (947+ cases, 0 failed) and time
`/tmp/orc/b_200_r.fun` (≈38 s now, expected <1 s).

**Oracle could not verify:** whether the prototype had the `d`/`e`/alias holes too (it rewrote only
entry types, so probably); whether rewriting `Evidence` is the *right* fix versus resolving bounds
differently (only that it works); the 5-channel patch was a one-off experiment, not a reviewed
implementation.

### Landed 2026-09-29 — merged to `main`, and what the ticket still owes

Steps 1–3 of the brief are done, in one source file (`src/Fun.Compiler/Elaborator.Patterns.cs`):
`Refined` factored out of `RefineContext` and applied to all seven channels, and
`RefinementTarget` now takes the level of the variable the scrutinee *evaluates to* (`VVar` with
an empty spine), null otherwise. Seven new cases, each proven on the unpatched tree first:
`type-case-{opened-entry, constructor-entries, constructor-entries-pattern, resume-entry,
self-methods, evidence, alias-scrutinee}`.

Integrator's own run on `main` after the merge: **954 cases, 0 failed** (947 + 7), xUnit
**208/208**, `dotnet build` 0 errors. Timing `/tmp/orc/b_200_r.fun` through the single-file
runner: **37.8 s → 1.02 s** (it previously blew the runner's 60 s elaboration budget outright).
`d.fun` and `e.fun` still fail with the same mismatch — untouched, no cases, as specified.

**Still open on this ticket — not code:**
1. The `d`/`e` ruling (branch-local types see the matched variable as the matched head?) —
   grilling, owed to the user. Both reproducers, preserved here because `/tmp` is ephemeral:

   ```fun
   // d.fun — y : T written inside the branch
   ({ f : [T : Type] -> T -> I64 = fn[T](x) {
        match (T) { I64 => { y : T = x; y + 1 }, _ => 0 } };
      f(7) })
   ```

   ```fun
   // e.fun — U = T, then y : U inside the branch
   ({ f : [T : Type] -> T -> I64 = fn[T](x) {
        U = T;
        match (T) { I64 => { y : U = x; y + 1 }, _ => 0 } };
      f(7) })
   ```

   Both fail `ELAB type mismatch: cannot unify VVar with VAtomTy(I64)` on `main` after the fix
   (expected `8`). Replacing `T`'s value slot fixes `d` but breaks `core-072`–`077` ("a meta's
   spine must be distinct variables"); `e` fails either way. `type-case-generic-programming.md:88`
   grants permission ("may treat the matched type variable as equal"), not a decision.
2. Whether rewriting `Evidence` is the *right* mechanism versus resolving bounds differently —
   green suite only shows it works.
3. ~~`Opened` vs `ConstructorEntries` are still rewritten together; which needs which is not
   isolated (probably both — patterns resolve through `ConstructorEntries`).~~ **Closed 2026-10-01** —
   ablated one channel at a time: term uses pin `Opened`, pattern heads pin `ConstructorEntries`,
   in opposite directions. See [Ablation](#ablation-fork-2026-10-01--every-channel-is-pinned-opened-and-constructorentries-are-distinct-paths-closes-item-3).
   The same run found every other channel pinned too (`Names` eleven times over); `SelfEntry` and
   `SelfMethods` both flip only `type-case-self-methods`, so whether one subsumes the other stays
   unisolated — nothing rides on it.
4. The mention index stays deferred: no measurement asks for it.

## Ablation (fork, 2026-10-01) — every channel is pinned; `Opened` and `ConstructorEntries` are distinct paths (closes item 3)

Method: for each of the seven channels in `RefineContext` (`src/Fun.Compiler/Elaborator.Patterns.cs`),
replace that channel's rewrite with a pass-through of the untouched channel (one line per channel;
the `ConstructorEntries` and `Evidence` blocks replaced whole — "skip that channel", the smaller
honest edit), rebuild, run the full suite, record failures, revert. `git diff` empty after every
run; nothing semantic is committed. Baseline and final: **954 cases, 0 failed** (xUnit 208/208),
same run in each ablation's own build.

| Channel | Ablating edit (in `RefineContext`) | Cases that failed | Verdict |
|---|---|---|---|
| `Names` | `Names = ctx.Names` | 11: `core-072`–`core-077`, `type-case-{refines-variable, alias-scrutinee, constructor-entries, constructor-entries-pattern, evidence}` | pinned (heavily) |
| `SelfEntry` | `SelfEntry = ctx.SelfEntry` | 1: `type-case-self-methods` | pinned |
| `ResumeEntry` | `ResumeEntry = ctx.ResumeEntry` | 1: `type-case-resume-entry` | pinned |
| `Opened` | `Opened = ctx.Opened` | 2: `type-case-opened-entry`, `type-case-constructor-entries` | pinned |
| `SelfMethods` | `SelfMethods = ctx.SelfMethods` | 1: `type-case-self-methods` | pinned |
| `ConstructorEntries` | `ConstructorEntries = ctx.ConstructorEntries` | 1: `type-case-constructor-entries-pattern` | pinned |
| `Evidence` | `Evidence = ctx.Evidence` | 1: `type-case-evidence` | pinned |

Every ablation fails at least one case: **no channel is uncovered, no new cases needed.** The
seven cases added by `63df1df` all earn their place, and `Names` is pinned ten times over beyond
them.

**Item 3 answered: `Opened` and `ConstructorEntries` are not redundant, and the two existing
cases distinguish them.** `OpenNominal` (`Elaborator.Enum.cs:183,188`) writes each opened
constructor into *both* channels, but the two consumers read different halves:

- A **term** use (`Some2(x)`) resolves the name to its entry and reads the entry's *type* — the
  `Opened` rewrite. Ablating `Opened` flips `type-case-constructor-entries` (and
  `type-case-opened-entry`); ablating `ConstructorEntries` does not touch it.
- A **pattern** head (`Some2(n)`) resolves through `LocateChoice` to the entry's *index* only —
  which `Refined` never changes (it rewrites `Type`, not `Level`) — and takes its binder types
  from `ConstructorEntries.Type` via `Instantiate` (`Elaborator.Enum.cs:229–233`). Ablating
  `ConstructorEntries` flips `type-case-constructor-entries-pattern`; ablating `Opened` does not.

So the ticket's guess is right that patterns resolve through `ConstructorEntries`, and wrong that
the two might be one path: term uses pin `Opened`, patterns pin `ConstructorEntries`, and the
distinguishing programs are the two cases themselves (`type-case-constructor-entries.fun` expect
`1`, `type-case-constructor-entries-pattern.fun` expect `8`) — each flips under exactly one
ablation, in opposite directions.

**Observations, not chased:** one case can pin two channels — `type-case-self-methods` fails under
both the `SelfEntry` and the `SelfMethods` ablation (mirror-image mismatches: `VVar` vs
`VAtomTy(I64)` and back). Whether one of those two subsumes the other was not isolated; a probe
would need a method whose type mentions `T` while `self`'s entry does not, which the enclosing
struct former may reject (the same shape the recon's first `SelfMethods` probe hit). Every
channel is already pinned, so nothing rides on it. `type-case-evidence` likewise fails under both
`Names` (its `x : A`) and `Evidence` — consistent, not an overlap of channels. No case failed
under an ablation it had no business depending on; nothing looked wrong with the `63df1df` fix.

No probe programs were written and no conformance cases added — the ablation exposed no gap.
