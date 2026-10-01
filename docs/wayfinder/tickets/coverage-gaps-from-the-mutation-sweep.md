---
title: The coverage gaps the mutation sweep found
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-10-01
resolution: "Closed 2026-10-01. All eight rows are resolved — seven cases landed across two forks (four then three, each proven by ablating the guard it names and seeing the case flip alone) and `ref-tuple-negative` closed as unflippable at both candidate guards with its probes recorded. Suite 954 → **961 cases, 0 failed**, xUnit 209/209, integrator-re-measured. Two findings about `src/` came out of it: `Nbe.cs:586`'s guard is unreachable by any program (its reachable sibling at `Nbe.Effects.cs:123` is cased instead), and `Primitives.cs:165`'s negative-arity refusal is only observable as a process-killing stack overflow."
assignee:
blocked_by: []
---

# The coverage gaps the mutation sweep found

Found 2026-09-28 by the wide mutation sweep recorded in
[the suite redundancy investigation](suite-redundancy-measured.md). The sweep's main answer was
negative — no deletion list is defensible — but **32 of its 70 mutations caught nothing**, and that
is a coverage finding rather than a suite-health one. Artifacts: `/tmp/mut/table.tsv` and
`/tmp/mut/fails/<idx>-<tag>.txt` (may be gone; the table below is the record).

## The 32, split by what the sweep thought they were

**Look like genuinely untested behaviour** (the ones worth cases):

| mutation | area | what is untested |
|---|---|---|
| `ref-budget-limit` | `Budget.cs` | the evaluator's **limit** check — no conformance case reaches it. (Budget *errors* are cased as `rec-divergent-*`; this is the specific limit path.) |
| `ref-refs-nonreference` | `Elaborator.Refs.cs` | a reference to something that is not a reference |
| `ref-expander-macro-provisional`, `ref-macro-position`, `ref-macro-arity` | `Expander.Macros.cs` | a macro used before it is complete, in the wrong position, or at the wrong arity |
| `ref-enforest-empty-block`, `ref-enforest-block-export` | the enforester | an empty block, and an `export` in a block |
| `nbe-effects-row-dedup` | `Nbe.Effects.cs` | a duplicate effect surviving row normalisation |
| `nbe-generative-decl-match`, `nbe-traits-op-index` | `Nbe.*` | two same-shape distinct declarers; a trait operation index |
| `ref-pattern-syn-arity`, `ref-type-arity`, `ref-tuple-arity`, `ref-tuple-negative` | reflection | pattern-synonym, type, tuple and negative-arity helpers — only *type-parameter over-supply* has a case |
| `export-last-member`, `syntaxmap-operator-scope`, `expander-roles-attaches` | expander | three unexercised paths |
| `ref-nbe-continuation-used` | `Nbe.cs` | reading back a used continuation |

**Look redundant with another check** — a *code* question, not a test one: `ref-effects-poly-arrow`,
`ref-effects-nonexhaustive`, `ref-selopen-unknown-member`, `ref-export-clash`,
`ref-elaborator-tuple-len`, `ref-traits-implhead-field`, `ref-dup-member`, `ref-unknown-method`,
`ref-dup-bound`, `ref-missing-impl` (each answers to a sibling check that already refuses the same
input, so a case could not tell them apart).

**Too small to decide a case** (leave them): `mc-fieldscover`, `mc-pin-covers`,
`mc-universe-covers` — the structural `Covers` relation never decides a case on its own.

## What a fork should do first

Five to six cases, in this order, all `error`-expecting and all in
`test/conformance/cases/elaborate/` or `cases/values/` as the subject fits:

1. `ref-budget-limit` — the limit path, which nothing reaches.
2. `ref-refs-nonreference` — the ref refusal.
3. `ref-macro-arity` and `ref-macro-position` — user-visible macro diagnostics.
4. `ref-enforest-empty-block` — an enforester refusal, cheap to write.
5. `nbe-effects-row-dedup` — an evaluator path, if a program can reach it.

**Every case must be proven to catch something**: re-apply the mutation it was written for and show
the new case fails, then revert. A case that pins a refusal the mutation cannot remove is worth less
than the mutation that motivated it, and this ticket exists because that went unmeasured for 848
cases.

## What this ticket is not

- **Not a deletion list.** The sweep's zero-catch counts are a statement about its own 19 flips, and
  [the investigation](suite-redundancy-measured.md) explains why they cannot convict a case.
- **Not message-pinning.** A conformance `.expect` is a value or the literal `error`; the exact
  message belongs in xUnit (`CLAUDE.md`). These cases assert *that* the refusal happens — which is
  precisely what the mutations showed was unasserted.
- **Not exhaustive.** The sweep could not mutate `Driver`, `Loader`, `Reflection`, `PreludeAbi`, the
  `Core.*` traversals, deeper `Unify`, `Nbe.Structs`/`Nbe.Rec`, `Expander.Imports`, `Reader` beyond a
  caret flip, or `Syntax.Map` beyond one flip, so their cases are unmeasured rather than safe.

## Landed 2026-09-28, and two findings from the writing

Two cases, both proven by removing their guard and showing the case fail:

- `elaborate/refs-nonreference` — `{ fn() { deref(5) }; 1 }` → `error` → `Elaborator.Refs.cs:80`.
  **The brief's own program would not have earned its place:** `{ deref(5) }` survives the
  mutation, because a *second* guard (`Nbe.Refs.cs:61`, the runtime refusal) refuses the same input
  and the case still sees `error`. Deferring the `deref` inside an *uncalled* lambda makes the
  mutant succeed (`VALUE 1`) and the case fail. **A case only earns its place if removing the guard
  it names flips it** — a sibling guard on the same input can mask the one you wrote for.
- `macros/macro-arity` — `{ macro m(x) { quote(x) }; m(1, 2) }` → `error`, which pins the
  **enforester**'s arity check (`Enforest.Macros.cs:102`), *not* the expander's the sweep named.

**The sweep's `macro-arity` target is not reachable by a conformance case.** Disabling
`Expander.Macros.cs`'s arity guard leaves the output byte-identical, so it is either dead or
answered earlier; a three-parameter operator macro (`infix (+++) (a,b,c) { a }; 1 +++ 2`) does reach
it, but removing the guard only changes the **message** (to `macro +++ did not return syntax`) and
the program still `error`s — unflippable, and `CLAUDE.md` puts message assertions in xUnit. So that
guard's target is an **xUnit** test, not a case here.

Still to write from the list: `macro-position`, `enforest-empty-block`, `nbe-effects-row-dedup`
(whose proof is done: `knownEffects.Add(e)` re-inserted → `type mismatch: effect rows do not agree`),
and `ref-budget-limit` (prove it by *lowering* the threshold, not by hunting a huge `N`).

## The six are resolved, 2026-09-28 (base `13ebfbd` → `059d15a`)

Five landed as cases, each **proven by removing its guard and showing the case fail**; the sixth
resolved as already-covered elsewhere. Suite `944` → **`947` cases, 0 failed**; xUnit `208`; no
commit touched `src/`.

| case | program | `.expect` | guard | the flip |
|---|---|---|---|---|
| `elaborate/ref-budget-limit` | `{ rec loop : I64 -> Type = …; g = fn(y : loop(200000)) { 1 }; 2 }` | `error` | `Budget.cs:113` (`_remaining <= 0`) | guard disabled → `VALUE 2`; restored → the budget error. Cross-checked by lowering `DefaultLimit` to 1000, where `N = 70` errors. |
| `macros/macro-position` | `{ macro e(_) : Expr(_) { quote { pub x = 1 } }; M = module { e(0) }; M.x }` | `error` | `Expander.Macros.cs:239` (`entry.Position != position`) | guard disabled → `VALUE 1`; restored → the refusal. |
| `values/nbe-effects-row-dedup` | the effect-row program above | `0` | `Nbe.Effects.cs:166` | dedup removed → `type mismatch: effect rows do not agree`; restored → `VALUE 0`. |
| `macros/macro-arity` | `{ macro m(x) { quote(x) }; m(1, 2) }` | `error` | the **enforester**'s check (`Enforest.Macros.cs:102`) | pins the enforester, not the expander the sweep named. |
| `elaborate/refs-nonreference` | `{ fn() { deref(5) }; 1 }` | `error` | `Elaborator.Refs.cs:80` | deferred inside an uncalled lambda so the mutant can succeed. |

**Two findings, one of them closing a "gap" as false:**

1. **`enforest-empty-block` is not a gap.** Disabling `Enforest.cs:55` falls through to the sibling
   `ParseAll` refusal (`expected expression`), so only the *message* changes and no conformance case
   can be flipped — and the message is **already** pinned in xUnit
   (`ExpandTests.cs:93`, `[InlineData("{ }", "empty block")]`). The sweep's zero-catch row measured
   the runner's `.expect` weakness, not missing coverage.
2. **`macro-position` had to reverse its program.** The brief's Decl-in-Expr case was masked — only
   the refusal's *message* changed — so the case was written the other way round (an `Expr` macro
   used where declarations go), mirroring what `refs-nonreference` needed. **A sibling guard on the
   same input is the standard trap here**: a case is only real if removing the guard it names flips
   it, and the first thing to check is whether another guard refuses the same program.

**Still unwritten from the list** (the sweep's other "untested" rows) — **all closed the same day: see the
second pass below.** The four it left (`syntaxmap-operator-scope`, `expander-roles-attaches`,
`ref-nbe-continuation-used`, `ref-tuple-negative`) landed as three cases plus one unflippable row.

## The remaining eight rows — fork `wright`, 2026-10-01
Four cases landed, each **proven by disabling its guard, showing the case flip, and reverting** (the
branch touches no `src/` file). Each mutation was the named guard neutralised (`if (…)` folded to
`false`, or `Last` → `First` for the export row) and observed through the single-file runner.

| case | program (abridged) | `.expect` | guard | the flip |
|---|---|---|---|---|
| `values/ref-pattern-syn-arity` | synonym `First(a)` used as `M.First(x, y)` | `error` | `Elaborator.Patterns.cs:234` | `this pattern synonym takes 1 arguments…` → `VALUE 3` |
| `values/ref-type-arity` | `classify(Opt(I64))` under `{ Opt(I64, Bool) => 1, _ => 0 }` | `error` | `Elaborator.Patterns.cs:411` | `this type takes 1 parameters…` → `VALUE 1` (the wrong-arity arm *matches*) |
| `values/ref-tuple-arity` | `classify(Tuple(2, I64, Bool))` under `{ Tuple(2, a) => 1, _ => 0 }` | `error` | `Elaborator.Patterns.cs:466` | `Tuple(2, …) takes 2 component types…` → `VALUE 0` |
| `values/export-last-member` | `N = module { pub rec T = enum { T(I64), Y }; export T }; M = module { export N.{T} }; M.T(3)` | `T` | `Elaborator.Export.cs:24` (`members.Last`) | `VALUE T` → `applying non-function` (`First` takes the nominal). Control `N.T(3)`, which does not use the selection, stays `T`. |

Rows **not** written, and why:

1. **`ref-tuple-negative` is unflippable at both of its candidate guards.** (a) `Elaborator.Patterns.cs:475` is unreachable from written source — a negative literal in a pattern is refused earlier by the enforester (`unsupported pattern`; in expressions `unsupported prefix operator: -`), so only a macro-constructed pattern could reach it — and it is **masked by sibling `:466`**, which fires for every negative count (`patterns.Count >= 0 != n`). (b) The term-level twin `Primitives.cs:165` (identical message) is reachable via a computed negative (`Tuple(0 - 5)`), but disabling it either leaves another `error` (`type mismatch: cannot unify VPi with VU` — the mutant type is a shallow `VPi`) or **StackOverflows the whole process**: the mutant value is the infinitely recursive `Type -> tuple_arity(n-1) -> …`, which every consumer either re-refuses or force-walks to death (probes: `{ y = Tuple(0 - 5); 1 }`, `{ g = fn(x) { x }; g(Tuple(0 - 5)); 1 }`, runtime-computed variants — all `Stack overflow.`). A case would "flip" only by killing the suite. No case added.
2. **`ref-nbe-continuation-used`: `Nbe.cs:586` (`cannot quote continuation`) is unreachable by any program** — a finding about `src/`, matching its own comment ("no probe read one back"). The continuation is bound anonymously (`Elaborator.Effects.cs`, `argCtx.BindAnonymous(contType)`); only `resume` reaches it, so no program can name it into a read-back value: a lambda `fn(x) { resume(x) }` reads back by *applying* the closure (the cont runs, never quotes), and returning it needs an infinite answer type (probe: `rec B = enum { Box(I64 -> B), Leaf(I64) }; … k => B.Box(k)` — `k` is the operation's *argument* binder, not the continuation). The same mechanism's reachable guard — the one-shot refusal `Nbe.Effects.cs:123` (`continuation already used`, with `:124` `Used = true`) — flips on `{ effect Ask = sig { ask : I64 -> I64 }; match (perform Ask.ask(1)) { n => n, effect Ask.ask k => { resume(1); resume(2) } } }` (`error` observed as `EVAL continuation already used`). **Its case is not yet written/proven at fork cutoff — the one thing to do next.**
3. **`syntaxmap-operator-scope` (`Syntax.Map.cs:168`, `Operator = m.Id(u.Operator)`) not reached at cutoff.** Only already-enforested `OperatorUse` syntax observes this line (rule replacements, quote templates, macro-emitted binding chains); `ExpandBindings` maps raw items by *token* through `MapToken`, a different line, which is why block-level role shadowing does not see it. Planned probe: a `macro … : List(Decl) { quote { infix (+++) ($a, $b) { … }; … } }` emitting an operator-macro declaration and a use of it in one splice.
4. **`expander-roles-attaches` (`Expander.Roles.cs:46`) — two readings, neither confirmed at cutoff.** (a) Disabling the exemption itself trips the *prelude*: `std/lib.fun:41` attaches fixity-only roles (`==` …) to values, so **every** program dies with `` `==` is both a syntactic role and a value binder where both are visible `` — the sweep's zero-catch claim is impossible for this reading, so the sweep's mutation cannot have been it. (b) Dropping the `attaches &&` conjunct (so the refusal never fires over a value) is the reading a case could pin specifically; `elaborate/role-conflicts-with-value-binder.fun` was located right at cutoff and likely already pins that refusal — verify it under reading (b) before writing anything.

Counts: suite `954 cases, 0 failed` measured before, `958 cases, 0 failed` after the four cases (clean-tree run; one intermediate run was against a stale mutated DLL and is discarded); xUnit not re-run at fork cutoff.

### Integrator's verification (2026-10-01, merged `79ee1f1`)

Re-measured on `main` rather than taken from the table above:

- baseline `958 cases, 0 failed` exit 0; `dotnet test test/Fun.Tests` → **209/209**; after both
  mutations were reverted, the same `958 cases, 0 failed`, tree clean.
- `Elaborator.Export.cs:24`, `members.Last` → `members.First`: build exit 0 →
  `values/export-last-member.fun` prints `ELAB applying non-function` → **`958 cases, 1 failed`**,
  exit 1, and that case **alone**.
- `Elaborator.Patterns.cs:466`, the arity check neutralised by appending `&& false`: build exit 0,
  zero `error CS` → `ref-tuple-arity` prints **`VALUE 0`** → **`958 cases, 1 failed`**, that case
  alone.
- Under each mutation the *other* three cases kept their own refusals, so the four pin four
  distinct guards rather than one guard four times.

**A note on how a wrong "unflippable" verdict gets made.** My first attempt at the tuple row
replaced the `if (expected is int n && …)` condition with `false`, which put `n` out of scope:
`dotnet build` exited **1**, `dotnet run` printed "The build failed", and the run measured nothing.
The honest verdict from that run would have been "the guard cannot be flipped" — the same shape as
the discarded stale-DLL run above. Any ablation must print its build's exit code *before* its
observation counts.

Rows 2–4 above were closed by a second pass on the same day — see
[the section below](#the-four-rows-left-by-the-2026-10-01-fork--closed-2026-10-01-fork-wright-second-pass) —
and row 1 stays closed as unflippable with the two probes recorded.

## The four rows left by the 2026-10-01 fork — closed, 2026-10-01 (fork `wright`, second pass)

Three cases landed, each proven the same way as the four above (named guard neutralised, `dotnet build`
seen succeed, single-file run showing the flip, revert, rebuild). The branch touches no `src/` file.

| case | program (abridged) | `.expect` | guard | the flip |
|---|---|---|---|---|
| `values/ref-nbe-continuation-used` | `match (perform Ask.ask(1)) { n => n, effect Ask.ask k => { resume(1); resume(2) } }` | `error` | `Nbe.Effects.cs:123` (`if (cont.Continuation.Used)`) | `EVAL continuation already used` → `VALUE 2` |
| `macros/syntaxmap-operator-scope` | macro emits `quote { infix (+++) (x, y) { x }; pub y = 1 +++ 2 }` as one `List(Decl)`, splice used as `M.y` | `1` | `Syntax.Map.cs:168` (`Operator = m.Id(u.Operator)`) | `VALUE 1` → `` ELAB `+++` is an operator macro with no macro of its name `` |
| `elaborate/expander-roles-attaches` | `{ answer = 5; syntax answer { answer => 42 }; answer }` (the twin case's two decls swapped) | `error` | `Expander.Roles.cs:46` (`attaches &&` conjunct dropped) | `…is both a syntactic role and a value binder…` → `VALUE 42` |

Row by row:

1. **`ref-nbe-continuation-used` — answered against the reachable sibling.** The sweep's *named* guard
   `Nbe.cs:586` (`cannot quote continuation`) stays recorded **unreachable** on the previous fork's
   evidence (its probes, and the code's own comment "no probe read one back"); this fork did not re-probe
   it. The case pins the reachable one-shot refusal `Nbe.Effects.cs:123`, proven by folding its `if` to
   `false`. The row is closed: the mechanism has one case on the guard that can actually fire.
2. **`syntaxmap-operator-scope` — landed, but not in the shape the previous fork designed.** The designed
   template-rule probe (`quote { infix (+++) ($a, $b) { $a + $b }; pub y = 1 +++ 2 }`) is **blocked at
   template read**: `$a` lands in binder *and* expression position — `the quote hole $a stands in
   positions of different kinds` — so a value-computing operator rule (whose operands must be `$`-spelled)
   cannot be quoted at all. Two more dead ends before the working shape: plain-id operands
   (`(x, y) { x + y }`) make the rule an *operator macro* whose body must return syntax
   (`macro +++ did not return syntax` when it returns a value), and a constant body
   (`($a, $b) { 42 }`) dies `unbound variable: a`. The shape that quotes cleanly is the operator macro
   returning a syntax argument (`(x, y) { x }`), infix decl and use in one `List(Decl)` quote — the case
   above. Two near-misses worth knowing: the same quote with a broken body flips only *message-to-message*
   (`macro +++ did not return syntax` → `` operator macro with no macro of its name ``) — both `error` at
   conformance granularity, so unflippable there; and a *rule-replacement* probe (template `$x +++ $x`
   defined under `+++ = -`, used under `+++ = +`) does **not** flip under the mutation at all (stays
   `VALUE 0` both ways) — that reacher of line 168 is unobservable through a value.
3. **`expander-roles-attaches` — the twin case does not pin the reading; a new case does.**
   `elaborate/role-conflicts-with-value-binder.fun` binds `syntax answer` *before* `answer = 5`, so its
   refusal fires on the value binding over an existing role, where `attaches` is `false` either way
   (`Expander.cs:39`) — dropping the `attaches &&` conjunct leaves it `error` (measured). What the
   conjunct gates is the other order: a non-attaching role bound over an existing visible *value*
   (`BindRoleAt` → `role.Attaches = false` for `syntax`). The new case is the twin with the two decls
   swapped. Reading (a) (disabling the exemption itself, tripping the prelude at `std/lib.fun:41`) stands
   as the previous fork measured it, not re-run.
4. **`ref-tuple-negative` — settled unflippable, no probe spent.** Per the previous fork:
   `Elaborator.Patterns.cs:475` is unreachable from written source and masked by sibling `:466`, and
   `Primitives.cs:165`'s removal yields only `error` or a stack overflow. This fork found no
   written-source spelling for a negative pattern arity (the enforester refuses `-` in patterns) and so
   spent no probe, as instructed. No case.

Counts: suite `958 cases, 0 failed` given as the baseline (not re-run before the three cases),
`961 cases, 0 failed` measured clean after (the three cases also each run green through the single-file
runner). Guards shown unreachable overall: `Nbe.cs:586` (per the previous fork, plus its own comment).

### Integrator's verification of the second pass (2026-10-01, merged `7868b73`)

Re-measured on `main`, each ablation with its build's exit code printed *before* the run counted
(the discipline this ticket's own record earned the hard way):

- baseline `961 cases, 0 failed` exit 0 → restore identical, tree clean → xUnit **209/209**.
- `Nbe.Effects.cs:123` folded to `if (false)`: build exit 0 → `ref-nbe-continuation-used` gives
  **`VALUE 2`**.
- `Syntax.Map.cs:168`, `Operator = m.Id(u.Operator)` → `u.Operator`: build exit 0 →
  `syntaxmap-operator-scope` gives `` ELAB `+++` is an operator macro with no macro of its name ``.
- `Expander.Roles.cs:46`, the `attaches &&` conjunct dropped: build exit 0 → the **new** case gives
  **`VALUE 42`** while the pre-existing `role-conflicts-with-value-binder.fun` **keeps refusing** →
  `961 cases, 1 failed`, that case alone. That pair is the evidence the new case is not a duplicate
  of the one already in the suite, which was the second pass's headline correction.
