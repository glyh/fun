---
title: "Port: effect, trait and impl statements inside a quoted block"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: effect, trait and impl statements inside a quoted block

Verdict of [the unverified rows](port-unverified-rows.md), row 7 — a **real gap**.
Verified by the integrator 2026-09-24 at `32aa27e`, both shapes.

## The programs

```fun
{ macro m(_) { quote( { effect E = sig { op : Unit -> Unit }; 1 } ) }; m(0) }
{ macro m(_) { quote( { trait T(A) = sig { f : A -> I64 }; 1 } ) }; m(0) }
```

| runner | output |
|---|---|
| OCaml | `1` for both |
| port | `not ported yet: reading the statement EffectDef as quoted syntax` / `… TraitDef …` (`Enforest.Roles.cs:867`) |

Note the shape: the reachable form is a quoted **block expression**, `quote( { … } )`.
`quote { … }` (braces, no parens) goes through `ReadItemsNow`/`ParseModuleStatement` and
never reaches `WithBody`, so a case written that way proves nothing — the probe found
this the hard way.

## Fix

`Enforest.Roles.cs:867`'s `WithBody` switch already returns `d with { Body = body }` for
`Syntax.SyntaxDef` and `Syntax.MacroDef`; `Syntax.EffectDef`, `Syntax.TraitDef` and
`Syntax.ImplDef` already carry a `Body` field too, so each is one more case line. Check
each kind against `ReadBlock`'s statement reading rather than assuming symmetry: the
probe only exercised `effect` and `trait`, so `impl` is untested.

## Tests

- Both programs above, ordinary shared cases (`expect` `1`; the prototype answers `1`).
- The `impl` shape, probed in both runners first — if the prototype refuses it, it is a
  divergence to list rather than a case to commit.

## Reading

- `dotnet/src/Fun.Expand/Enforest.Roles.cs:845` (`ReadBlock`) and `:867` (`WithBody`)
- the `Syntax.*Def` variants in `dotnet/src/Fun.Kernel` / `Syntax.cs`
