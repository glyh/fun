---
title: "Port: the probed rows' conversions — one FunException and two assertions"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
assignee:
blocked_by:
---

# Port: the probed rows' conversions — one FunException and two assertions

The mechanical half of [the unverified rows](port-unverified-rows.md): three sites the
probing settled as *parity* or *unreachable*, so nothing here is a feature. Landed
together because each is a line or two and they share one verification run.

All three verified by the integrator 2026-09-24 at `32aa27e`.

## 1. Parity — `Syntax.Stx` must be a language error (row 3)

```fun
{ macro m(_) { Syntax.RawStx(None, Syntax.i64(7)) }; m(0) }
```

| runner | output |
|---|---|
| OCaml | `Failure("stx-only syntax should not reach elaboration")` (`elab_infer.ml:1418`, an assertion) |
| port | `not ported yet: elaborating Stx` (`Elaborator.cs:304`) |

Both refuse — the marker is reachable only through the reflected `RawStx` builder
(`std/stage1.fun:32`); the ordinary typed-argument path never produces one. So the port's
throw is wrong about the language and becomes
`case Syntax.Stx: throw new FunException("stx-only syntax should not reach elaboration")`,
with the default left for other kinds. A control — `{ macro m(x : Expr(I64)) : Expr(I64)
{ x }; m(3) }` — answers `3` in both, so the ordinary path is untouched.

## 2. Unreachable — `Unify.cs:222`'s catch-all (row 4)

The probing fork could not put a `VRef`, `VCont` or `VPatternSynonym` into a meta
solution; metas are solved with *types*. Make the catch-all an
`InvalidOperationException` (an assertion), not a "not ported yet". `Unify.cs:204`'s
refusal for those three kinds mirrors the prototype (`unify.ml:204-206`) and **stays**.

## 3. Unreachable — `Nbe.cs:577`'s catch-all (row 5)

No quoting of a `VCont` was reachable either (tried: `resume` through nested lambdas, a
module value, a struct value, a closure result). The prototype's `nbe_quote.ml:181` is an
`EvalError`, so prefer parity with it — `Value.VCont => throw new FunException("cannot
quote continuation")` — and leave the remaining kinds an assertion. Probe the prototype
first if the distinction matters.

## Tests

No new shared cases: each change is asserted by the site no longer being reachable, so
the verification is the full suite green plus, for 1, an `error` case if one does not
already exist for the `RawStx` shape (probe both runners: the prototype errors too, so it
would be an ordinary `error` case, not a divergence).

## Reading

- `dotnet/src/Fun.Compiler/Elaborator.cs:304`, `Unify.cs:202-206`/`:222`, `Nbe.cs:577`
- `test/conformance/prototype-divergences.txt` (nothing to add here)

## Resolution (2026-09-24) — closed

The three conversions were finished before the work pause and merged after it; the pause
held them only between the fork finishing and this verification. Branch
`port-probed-row-conversions`, commit `0b71c6d` (base `1dec417`), merged as `6ac05db`.

- `Elaborator.cs` — `Syntax.Stx` now throws
  `FunException("stx-only syntax should not reach elaboration")`; the default stays for the
  other kinds.
- `Unify.cs` — the catch-all is now an `InvalidOperationException` ("unhandled
  solution"); `:204`'s three-kind refusal is untouched.
- `Nbe.cs` — the catch-all is an `InvalidOperationException`, plus
  `Value.VCont => throw new FunException("cannot quote continuation")` for parity with
  `nbe_quote.ml:181`.
- One new **ordinary** case, `macros/raw-stx-not-elaborated` (`expect` `error`); the
  divergence list is unchanged at 26 and the control `m(3)` still answers `3` in both.

**Verified by the integrator after merging:** C# 728 → **729 cases, 0 failed**;
xUnit 182/182; `dune test` and `dune test test/conformance` green — 729 cases, 0 failed,
26 divergences. None of the three sites turned out reachable: the new case exercises the
`Stx` *language error* (as intended), and the two catch-alls remain unprobed, which is
what their assertions now say.

Closed 2026-09-24; the branch is pruned.
