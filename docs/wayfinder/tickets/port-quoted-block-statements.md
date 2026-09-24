---
title: "Port: effect, trait and impl statements inside a quoted block"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
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

## Resolution (2026-09-24) — closed

Merged from `pi-agent-5e131519-0819-4b9`. `WithBody` gained three case lines
(`Enforest.Roles.cs:869-871`), parallel to the existing `SyntaxDef`/`MacroDef` cases, so
`Syntax.EffectDef`, `Syntax.TraitDef` and `Syntax.ImplDef` each attach their body.

- **Cases added** (all ordinary, `expect` `1`): `macros/quote-block-effect`,
  `macros/quote-block-trait`, `macros/quote-block-impl`. Nothing was added to
  `prototype-divergences.txt` — the prototype answers `1` for each.
- **`impl` was probed before the case was committed**, because the probing round had left
  it untested: `{ trait Size(A) = sig { size : A -> I64 }; macro m(_) { quote( { impl
  Size(I64) = module { size = fn(n) { 3 } }; 1 } ) }; m(0) }` answers `1` in both runners.
  (The prototype needs the trait in scope or it reports `UnknownTrait "Size"` — ordinary
  scoping, not a divergence; the committed case declares it.)
- **Verified by the integrator after merging:** C# **733 → 736 cases, 0 failed** (the
  three new cases on a `main` that already had 733); xUnit 182/182; `dune test` and
  `dune test test/conformance` green — 736 cases, 0 failed, **27** divergences.
- Correctly left alone: the same statements reached through `quote { … }` *without*
  parens take the `ReadItemsNow`/`ParseModuleStatement` path, which never calls `WithBody`
  — that path already worked, and a case written that way would prove nothing.
- Not probed: a *named* `impl` (`impl name : T(A) = …`) inside a quote. The change only
  attaches the body, so it is shape-identical for both, but it stays unverified.
- The ticket's warning held up: this was three case lines and three cases, nothing more.
