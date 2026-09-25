---
title: "Port: reach feature parity with the prototype (the parity recipe)"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: reach feature parity with the prototype

The port's acceptance bar is **"C# covers every feature the OCaml prototype has"**,
and it is *not* the same thing as a green conformance run. There are two deltas:

- **The visible delta** — the failing shared cases: 13 of 690 (2026-09-18), each a
  singleton or near-singleton, triaged in [port-stage2-residue](port-stage2-residue.md).
- **The invisible delta** — the paths the port refuses with
  `NotImplementedException("not ported yet: …")`: **60 sites in 25 files** under
  `dotnet/src`, plus **15 `ponytail:` stopgaps**. The C# runner
  (`dotnet/test/Fun.Conformance/Program.cs`) counts a `NotImplementedException` as a
  failure *only when a case reaches it*, so a feature no shared case exercises is a
  silent hole. This is the list that actually answers the question.

## The instrument: one shared case, both runners

For any suspected gap, write `test/conformance/cases/<area>/<name>.fun` + `.expect`
and run both runners. Three verdicts, all useful:

1. **OCaml handles it, C# refuses it** — a real gap. Port it, keep the case.
2. **OCaml refuses it too** — *parity*, not a gap. Delete the C# throw and make it
   the proper `FunException` (porting convention 2), keep the case as `error`.
3. **Unreachable** — delete the throw; the catch-all is lying about what exists.

Verified examples of verdict 2, which is why the audit precedes the porting:
`Enforest.cs:412` "bracket expressions" — the prototype refuses bare brackets too
(`lib/expand/enforest.ml:87`, "not in Phase 7A"); `Enforest.Roles.cs:338` "a dotted
order group reference" — an open item on
[brackets-decide-grouping](brackets-decide-grouping.md), so the prototype lacks it.

Never commit a case that the port cannot pass (porting convention 8: the conformance
count never drops). A gap-closing fork adds the case; an audit fork keeps the program
in its report.

## Order

**1. Finish E11 — nominal identity.** The one place the port is *less correct* than
the prototype rather than merely incomplete, and the widest blast radius.
`values/elab-062` (expects `10`, gets `0`), `values/core-067` (a generative type
escapes its binder), and the "constructor pattern head on a generative nominal"
refusal all want the same machinery. Both `Elaborator.cs:368` (module stamp slot) and
`Elaborator.Enum.cs:48` (declaration identity) record that the slot list is ready for
it, so no index moves by hand.
→ [port-nominal-identity](port-nominal-identity.md)

**Done 2026-09-20 — C# conformance 695 cases, 0 failed**, xUnit 172/172, `dune test`
green. One private stamp slot per module, type-case by declaration + captures + stamp,
sealed heads resolved through the sealing context. The ticket's guess about `elab-062`
was **wrong**: it was not the nested-`Eval` stopgap but a missing closure case in the
captures comparison, so `a.T` did not equal itself — diagnose-before-fixing earned its
keep. Four shared cases now cover the generative half, which had none. **Done 2026-09-24**
(`921da47`+): the last E11 shape, a **parametric** nominal in a generative module, landed
([port-generative-former-nominal](port-generative-former-nominal.md), C# 721 → 723/0,
xUnit 182/182, both runners green), and what it does not reach is now
[the generative former's identity residue](port-generative-former-identity-residue.md) —
a type-case still cannot separate two instances of a former (prototype `10`, port `11`),
and a generative former with an *unused* type parameter is refused where the prototype
accepts (that half likely needs a ruling: the prototype treats the parameter as phantom).

**A green suite is not parity — and it is now green.** 695/0 is the visible delta
closed; the 17 real gaps from step 3 are what remains, and not one of them is visible
to the suite as it stands.

**2. The residue's remaining families**, in the residue ticket's own order:
record method calls (4), `Eq` impl resolution (2, including the grilled generic-impls
ruling on [trait-op-takes-innermost-impl](trait-op-takes-innermost-impl.md)),
`elab-059` / `elab-067` (one missing `switch` case each), then `core-102`, `core-270`,
`core-165`. → [port-stage2-residue](port-stage2-residue.md)

**Done 2026-09-20** (`fbea929`, `4fdab26`): 13 → **3**, xUnit 168/168 throughout, both
runners green. Nine fixed across five independent causes; `imports/core-165` was a
shared case that could only pass via the by-name constructor resolution the port
rejected, so it was **rewritten to say what it means** rather than ruled on, and the
rejected shape got its own divergence case. The residue ticket is closed: the 3 that
remain are all E11 (`elab-062`, `core-067`, `elab-067` — the last is the `SymbolTable`
program, so it is not the local one-case fix its triage implied) and are
[port-nominal-identity](port-nominal-identity.md)'s.

Still open from that family: the grilled generic-impls ruling on
[trait-op-takes-innermost-impl](trait-op-takes-innermost-impl.md) is implemented
nowhere (no shared case exercises it), `Nbe.Convertible` remains a stopgap, and a
top-level `pub impl` still refuses the `pub` form.

**3. The unported-path audit** — classify all 60 throw sites and 15 `ponytail:` notes
into the three verdicts above, so the invisible delta becomes a number and the real
gaps become tickets. Expect it to *shrink* the work. → [port-unported-path-audit](port-unported-path-audit.md)

**Done 2026-09-20** (`fb67a63`, `e4f5320`): 62 sites — **17 real gaps, 13 parity
conversions, 17 unreachable, 9 undecided, 4 owned**. The audit's outcome, in the
order the frontier should take it:

- [implicit application `f{ e }` and `g[I64]`](port-implicit-application.md) — the two
  the audit *reproduced*. Verified again by the integrator; the only gaps with a
  known-passing prototype program, so the cheapest 2 cases on the board.
- [the parity throws become language errors](port-parity-conversions.md) — **done
  2026-09-20** (`d1d10fe`): 12 rows converted, 17 unreachable catch-alls became
  invariant failures, 11 error cases added, 695 → 706/0. It also **corrected the
  audit**: `Enforest.Roles.cs:343` (a dotted order-group path) is a real gap, not
  parity → [port-order-group-through-unit-path](port-order-group-through-unit-path.md).
  The live convention-2 violation (`{ 1 + 2 ~> 3 }`) is repaired.
- [latent form gaps](port-latent-form-gaps.md) — **G2 and G3 done 2026-09-20**
  (`1edd7af`): `Term.Map` is total and a reflection-based test now fails on any new kind,
  so a forgotten kind cannot recur; G3's unit token and parameter trait bounds are read;
  695 → 707/0, xUnit 172 → 178. What remains is split out, and none of it is a
  traversal: [the reflected Syntax ADT differs](port-reflected-adt-differs.md) (a
  trait's arity, a missing `TypeDef` node — needs a ruling), G4 and G5 unverified
  reachability, and [a stuck match waits](port-stuck-match-sub-occurrence.md) (ruled).
- [a pattern synonym is checked, and generalizes where its type is unknown](port-pattern-synonym-generalizes.md)
  — **ruled 2026-09-21**, while the undecided rows were being probed: a product or
  bare-binder right-hand side (`pattern Two(a, b) = (a, b)`, `pattern Id(x) = x`) is
  legal — the synonym is checked at its declaration and the types it cannot know are
  generalized like a generic function's, instantiated at the use; the prototype's
  `TupleLengthMismatch` / `NotANominalType` are prototype defects, and the port's
  `not ported yet` on that path (`Elaborator.Patterns.cs:71`) becomes work.
- The undecided rows are **settled** (2026-09-24, [the unverified rows](port-unverified-rows.md)
  now closed). Three port gaps came out of the seven rows, in the order they should be
  taken — [an enum's captures come from its payload values](port-enum-captures-from-payload-values.md)
  (the port **crashes** on it today, so the runner must also stop dying),
  [effect/trait/impl statements in a quoted block](port-quoted-block-statements.md), and
  [a pattern synonym over a type-case pattern](port-pattern-synonym-over-type-case-rhs.md).
  The mechanical half — one `FunException` and two assertions — is
  [the probed rows' conversions](port-probed-row-conversions.md). The fourth candidate
  was **deferred by ruling**: a *typed* operator macro is not port work, because parity
  there is "neither implementation has it" — the prototype hangs on it — so it became
  [post-port work](port-typed-operator-macro.md), and the port keeps its refusal.
- **Re-swept 2026-09-24** — the refusal inventory is down from the audit's 62 sites to
  **20 sites / 17 distinct messages**, every one owned by an open ticket, and the
  *prototype* side was swept for the first time, which is the half that cannot be seen from
  the port at all. It found [two budget behaviours](port-budget-observable-cases.md) and
  [four runtime-error variants](port-runtime-error-cases.md) that no case anywhere covers —
  the visible answer to "is the port a superset?": not knowably, and these are the places
  where nobody would notice if it were not. It also resolved one refusal family as
  **parity** (the trait/impl arity message) →
  [a language error, not an unported path](port-trait-impl-arity-message.md).
- The 9 undecided and the 13 parity rows are ruled by the two rules above; the ones
  that need a semantic ruling are one question at a time, per the port ticket. Three are
  now settled without one: the parametric nominal in a generative module (real gap), the
  stuck match (**the port is right** — a divergence to record), and the dotted
  order-group path (real gap, found by probing a row the audit had called parity).

**Internals parity — ruled (user, 2026-09-20): behavioural only, budget yes, shapes
no.** The port is complete when it agrees on every source → result case; add
`BudgetTests.cs` for the three observable budget cases; do not mirror the
tree-shape/scope-set/parser-combinator Alcotest suites, because the domain model
names the surface as an erasure plus one escape hatch and a port may build a
different tree. The audit's internals table closes on that basis.

**4. Close the recorded divergences.** Every case in
`test/conformance/prototype-divergences.txt` (19 lines, 14 tickets) is already correct
in C# — the file says so, and the C# runner passes all of them. The work is
verification and bookkeeping: confirm each still diverges as recorded, close the
tickets, prune them from the map. `trait-op-takes-innermost-impl` is the exception:
its grilled decision (a free name in an impl head binds) is not implemented anywhere
yet, so it belongs to step 2.

**Also part of 3:** internals parity. The conformance suite cannot see shapes, the
unifier, the machine or budget accounting, so C# coverage for those rests on
`dotnet/test/Fun.Tests` mirroring `test/syntax/*.ml` and `test/semantic/test_elaborate.ml`.
Check the mirror is complete (there is no `BudgetTests.cs`). **Audited 2026-09-20 and
ruled on: source → result parity plus the budget cases; the shape suites are not
mirrored.**

## Not parity work — do not start these for this goal

The prototype does not have them either: `macro-owns-its-output`,
[design-private-type-visibility-model](design-private-type-visibility-model.md),
[design-trait-library-deriving-and-protocols](design-trait-library-deriving-and-protocols.md),
[general-set-literals](general-set-literals.md),
[specify-stage-12-macro-diagnostics-and-expansion-ux](specify-stage-12-macro-diagnostics-and-expansion-ux.md),
[reflect-match-in-expr-macro-adt](reflect-match-in-expr-macro-adt.md),
[scope-enforester-improvements](scope-enforester-improvements.md),
[type-def-chains-in-scoped-do-heads](type-def-chains-in-scoped-do-heads.md),
[brackets-decide-grouping](brackets-decide-grouping.md)'s open tail, and the two
research tickets ([deep-non-tail-recursion-is-superlinear](deep-non-tail-recursion-is-superlinear.md),
[type-case-refinement-walks-whole-context](type-case-refinement-walks-whole-context.md)).

## Fork allocation

The user's standing rule is **at most two implementation forks at once**
([port-core-tt-to-dotnet](port-core-tt-to-dotnet.md#wave-3-2026-09-16--2026-09-17--merged-paused),
2026-09-16). The four steps above are therefore run as two implementation forks
(1 and 2) and two forks that write no feature code (3 and 4). Forks follow the
[porting conventions](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16);
a fork does not edit this ticket, the map, or `docs/STATUS.md` — it reports, and the
integrator records.

## The measured delta (2026-09-25)

`scripts/differential.sh` (with `bin/differential.ml` and the C# runner's `--file` mode) runs
every `.fun` program in the repo through both runners and compares outcomes by **class**
(`VALUE` / `OK` / `ELAB` / `EVAL` / `HANG`), adjudicating a disagreement against the case's
`.expect`. The first measured run:

| | count |
|---|---|
| enumerated | 804 files |
| ran | 752 |
| skipped — `<name>.unit-<unit>.fun`, each printed with its reason | 54 |
| **agreed** | **723** |
| **port fails, prototype answers** | **0** |
| prototype fails, port answers | 29 — every one already in `prototype-divergences.txt`, tagged |
| both fail | 0 |
| hang | 0 |
| runner error | 0 |

Measured at `1af90e0`; a fresh run on the final tree is in flight as this was written. Since
`1af90e0` the tree gained one case (`values/rec-enum-former-ignores-outer-name`, a prototype
divergence — the prototype over-captures), so the expectation is **30** prototype-fails with
agreement unchanged. Re-run rather than assume: it is one command.

**What this number does and does not mean.** It bounds *the corpus*: no program in this repo is a
case where the port fails and the prototype answers. It does **not** bound the language — every
port-side gap found on 2026-09-24/25 (the implicit-lambda gate, the reflected arities, the
pattern-synonym type-case head, the recursive-enum capture crash, the quoted-block refusals) was
found by *probing*, and none of those shapes exists as a repo program. Probes remain the
instrument for the language; this measures the corpus. The harness's own README (`scripts/README.md`)
records the normalization and what it cannot see (a constructor's spine, error wording, a
prototype that hangs).

**Before asking the user the two parked decisions** (the phantom type parameter, supplyable
pattern-synonym types), check the model first: the last question of that shape — a former's
captures — was already answered by footgun 6 of
[nominal identity](nominal-identity-applicative-by-purity.md), and asking cost more than reading.
**Done 2026-09-25** for the phantom parameter: footgun 6 was pointing at "the parameter means
nothing, so it is not part of the identity", and the user took the stricter route instead —
[an unused type parameter is an error at its declaration](port-generative-former-phantom-parameter.md),
in both implementations; queued behind a free fork slot.

**A stale gap, closed 2026-09-25.** `port-generative-former-identity-residue` was carried here
as "one remaining E11 gap"; its section 1 had already been fixed and merged on the same day the
ticket was written (`1f70e82`, merged `5a685ba`, with the case
`values/nominal-generative-former-type-case-separates`). Re-measured in both runners 2026-09-25:
`VALUE 10` each, `.expect` `10`, no divergence entry. The ticket is closed and the fork that
went looking for it correctly reported nothing to do — so this list is one item shorter than it
reads. Measure a gap in the runners before spending a fork slot on it.
