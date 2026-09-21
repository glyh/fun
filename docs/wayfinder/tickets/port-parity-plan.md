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
- [the parity throws become language errors](port-parity-conversions.md) — mechanical,
  and it repairs a **live** convention-2 violation: `{ 1 + 2 ~> 3 }` is answered with
  `NotImplementedException` where the prototype type-errors.
- [latent form gaps](port-latent-form-gaps.md) — G2 traversals (broadest reach),
  G3 the reflection reader, G4 typed operator macros, G5 rec-enum captures, G6 a
  stuck match. Latent: no shared case reaches them, so the case comes first.
- The 9 undecided and the 13 parity rows are ruled by the two rules above; the ones
  that need a semantic ruling are one question at a time, per the port ticket.

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
