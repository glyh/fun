---
title: "Port: generalising a pattern synonym over a stuck neutral"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-25
resolution: Closed 2026-09-25 - fixed by a fork (be3ea8e, merged) and verified by the integrator, including the prototype half the fork could not run (no _build in its worktree) - 757 cases 0 failed and 185/185 xUnit, dune test green at 757/0 with 31 divergences, the new case VALUE 1 in both runners. One residual recorded below.
assignee:
blocked_by:
---

# Port: generalising a pattern synonym over a stuck neutral

> ## Resolution (2026-09-25) — closed
>
> Fixed by a fork (`be3ea8e`, merged) and verified by the integrator — **including the half the
> fork could not run**: its worktree has no `_build`, so it could not execute the prototype, and
> it said so instead of claiming the case was green. Integrator runs: port
> `757 cases, 0 failed` (was 756) and `185/185` xUnit; `dune test` green at
> `757 cases, 0 failed, 31 divergences`; `values/pattern-synonym-over-stuck-neutral` answers
> `VALUE 1` in **both** runners, matching its `.expect` (`1`). Ordinary case, so
> `prototype-divergences.txt` is untouched.
>
> The fix is nine lines. `CollectSynonymMetas` gains two cases: `Value.VNeutral` walks the spine's
> `Frame.FApp` arguments (the head is a variable or a rigid atom and contributes no metas; `FProj`
> and `FDot` reach no value) and `Value.VEffectRowTy` returns without collecting. The audit had
> flagged `VEffectRowTy` as a likely second kind needing the same treatment; it carries no payload
> at all, so there was nothing to collect — the flag was right to check and wrong about the answer.
>
> **Residual, recorded rather than ticketed — no reaching program is known.** The port's collector
> is now *narrower* than the prototype's `elab_generalize.ml`, which collects through a flex spine
> while ignoring rigid atoms, and every other value kind still throws **by design**
> (`VEffectRow`, `VEffect`, `VLam`, `VGlued`, `VFix`, `VRecord`, `VCon`, `VPatternSynonym`, …).
> The failure mode to watch is a value kind that carries a meta in a **type** position rather than
> an argument position: that would be **missed silently** rather than refused, which is the one
> outcome this whole family of collectors exists to avoid. If a program ever shows it, the
> collector is where to look — and that is why it is written down instead of dropped.

Site 5 of the 2026-09-25 [re-sweep](port-unported-path-audit.md#re-sweep-2026-09-25) of the
refusal inventory, found by a read-only audit, re-probed by the integrator. Unowned: the ticket
that created this machinery ([port-pattern-synonym-generalizes](port-pattern-synonym-generalizes.md))
is closed, and this is the default-case residue it left.

## The program, and both runners

```fun
{ g = fn(T : Type) { match (T) { I64 => String, _ => I64 } };
  E = fn(T : Type) { enum { C(g(T)) } };
  M = module { pub pattern P(a) = E.C(a) };
  1 }
```

| runner | output |
| --- | --- |
| OCaml | `VALUE 1` |
| port | `ELAB not ported: not ported yet: generalising a pattern synonym over a VNeutral` |

`g(T)` is a type-level match **stuck** on the variable `T` (the scrutinee's head is unknown), so
the constructor's payload type is a neutral. The synonym's right-hand side cannot determine its
scrutinee's type from it, so its types are generalised — which is exactly what the closed ruling
says to do — and the port's collector gives up on the neutral instead. The prototype answers `1`,
so this is a gap, not parity.

## Where the port refuses

`dotnet/src/Fun.Compiler/Elaborator.Patterns.cs:258`, in the `CollectSynonymMetas` family: the
default case throws over the value it could not walk.

## What to do

1. **Walk the neutral, not just its head.** A stuck neutral is a head plus a spine; the metas that
   need generalising are in the spine, and the head is a variable or a rigid atom that contributes
   none. That is the shape the audit's fix direction names, and it is worth checking the same
   treatment for the other value kinds that can carry a type — the audit specifically flagged
   `VEffectRowTy` as a likely second one.
2. `CollectSynonymMetas` is `Elaborator.Patterns.cs`'s, so **this ticket is serialized behind the
   in-flight [sealed-nominal-head](port-pattern-synonym-over-sealed-nominal-head.md)** and ahead of
   [the synonym's implicit type parameters](pattern-synonym-type-parameters.md), which touches the
   same collector. Do not run it beside either.
3. **Test**: a shared case, `.expect` `1` — ordinary, since the prototype answers `1`. Keep the
   program above verbatim; it is the shape that reaches the default case, and it took the audit a
   probe to find, so do not "simplify" it without re-probing.

## Reading

- `dotnet/src/Fun.Compiler/Elaborator.Patterns.cs` — `CollectSynonymMetas`, `InstantiateSynonym`,
  and the `:258` default
- `dotnet/src/Fun.Kernel/Core.Patterns.cs` — `VPatternSynonym`, what a use site carries
- [port-pattern-synonym-generalizes](port-pattern-synonym-generalizes.md) — the ruling that says
  generalise where the type is unknown, and which readings it settled
- [a pattern synonym over a sealed-nominal head](port-pattern-synonym-over-sealed-nominal-head.md)
  — the sibling gap in the same file, and the one that must land first
