---
title: The coverage gaps the mutation sweep found
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
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

**Still unwritten from the list** (the sweep's other "untested" rows, none of them attempted): the
reflection arity helpers (`ref-pattern-syn-arity`, `ref-type-arity`, `ref-tuple-arity`,
`ref-tuple-negative`), `ref-refs-nonreference`'s siblings `export-last-member`,
`syntaxmap-operator-scope`, `expander-roles-attaches`, and `ref-nbe-continuation-used`.

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
