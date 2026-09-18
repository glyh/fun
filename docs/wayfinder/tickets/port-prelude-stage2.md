---
title: "Port: prelude stage 2"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: prelude stage 2

Follow the porting conventions in
[port-core-tt-to-dotnet](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16).

Item 1 of the [handover (2026-09-17)](port-core-tt-to-dotnet.md#handover-2026-09-17--start-here).
Expect the largest single jump in the conformance count: of the 296 failing cases,
199 fail on "the infix operator …" and 95 on "prelude syntax roles".

## Scope

- Make `dotnet/std/stage2.fun` **the** prelude. It already compiles as a unit
  (`InterleavingTests.Stage2CompilesAsAUnit`) and already opens with
  `Core = import "std"; export Core; open Core;`, so it re-exports stage 1 by
  construction. `Prelude.Binding` (`stdlib`) must resolve to stage 2, and
  `import "std"` with it.
- Its roles reach a program through the program's `open (import "std")`, exactly as
  stage 1's `if` does today: the order groups (`disjunction` … `negation`), the
  `infix`/`prefix` declarations for `&&`, `||`, `==`, `!=`, `<`, `>`, `<=`, `>=`,
  `+`, `-`, `*`, `/`, `%`, `not`, the `Eq` trait and its impls, and the stage-2
  syntax forms (`type`, and whatever else `stage2.fun` declares).
- Stage 1 stays a separate elaboration: stage 2 imports it as a unit, so stage 1 is
  still elaborated once per process against the builtins alone. Do not inline them.

## Delete the stopgaps in the same commit (decided 2026-09-18)

The user chose **A**: stage 2 becomes the prelude and every "stage 2 is not ported"
rule goes in the same commit, so each remaining failure is an honest pass or an
honest error. Two sources of truth for `type` and the operators must not coexist,
even briefly. Delete:

- `Prelude.cs`: the `Stage2` lazy, `Stage2Names`, `SpellsStage2Name`, and
  `PublicNames` — the whole token-scanning apparatus that guesses what stage 2
  would have supplied. Fix the class doc comment ("Stage 2 is not ported").
- `Driver.cs` `Elaborate`: the `catch (ExpandException)` branch's
  `SpellsStage2Name` test and its `NotImplementedException("not ported yet: prelude
  syntax roles …")`. An `ExpandException` becomes a plain `FunException` again.
- Any other site that reads stage 2's names rather than its bindings
  (`git grep -n 'Stage2\|SpellsStage2Name'`).

The count may dip before it jumps — a case that used to report "not ported yet" and
so counted as an expected failure may now report a real error. That is the point.

## Decided rules to read first

Domain model I5 ("What the base context holds"); glossary **Base context**,
**Prelude**, **Compilation unit** (`stdlib` is *bound*, not opened);
[port-prelude-stage1](port-prelude-stage1.md) for how stage 1 was wired and which
of its deviations are unverified; `STATUS.md` "Staged prelude; `type` is a std
macro; `export`". Supporting material only: the prototype's
`Elab_prelude.stage2_source` (`lib/semantic/typecheck/elab_prelude.ml:107`) and
`Elab_entry`/`Macro_driver.std_syntax`.

## Tests

- `InterleavingTests.Stage2CompilesAsAUnit` keeps passing (or is replaced by the
  stronger statement that stage 2 *is* the prelude).
- Conformance: report the count before and after, and the FAIL-list diff. No case's
  `.expect` may be edited to make it pass.
- xUnit stays 172/172 or better.

## Triage (item 3 of the handover)

Whatever stage 2 leaves failing gets triaged into tickets — not fixed in this
commit. Group the residue by error text and file one ticket per cause. A prototype
defect found on the way is reproduced, ticketed, fixed **in C# only**, and listed in
`test/conformance/prototype-divergences.txt`.

## Grilled (2026-09-18)

**`stage2.fun`'s own import of stage 1.** `dotnet/std/stage2.fun` opens with
`Core = import "std"; export Core; open Core;` — it imports stage 1 under the same
unit name `"std"` that stage 2 is about to become, so once `"std"` resolves to
stage 2 that line points at itself.

**Ruling: stage 1 gets its own unit name.** `"std"` means stage 2 and nothing else;
stage 2 reaches stage 1 through a distinct name (`"std/stage1"`, or whatever spelling
the loader's unit keys already use — pick the one consistent with `Prelude.Path` and
`Loader`, and say which in the resolution). One unit name, one unit.

Rejected: resolving `"std"` per asker (stage 1 to stage 2, stage 2 to programs). That
makes `import "std"` mean two different units depending on who writes it — a rule with
no name in the glossary, and the exact kind of unwritten invariant
[domain-model-core-tt](domain-model-core-tt.md) exists to stop the port from
inheriting.

Cost accepted: this one line of `dotnet/std/stage2.fun` diverges from the prototype's
`Elab_prelude.stage2_source`, which has no unit loader and so has no such collision.
Note the divergence in the resolution; it is a port-only difference, not a language
change, so it does not belong in `prototype-divergences.txt`.
