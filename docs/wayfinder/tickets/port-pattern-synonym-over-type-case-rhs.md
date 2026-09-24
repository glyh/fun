---
title: "Port: a pattern synonym over a type-case pattern"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: a pattern synonym over a type-case pattern

Verdict of [the unverified rows](port-unverified-rows.md), row 6 — its remaining half.
The other half of row 6, the product and bare-binder right-hand sides, is settled by the
user's ruling in [a pattern synonym generalizes](port-pattern-synonym-generalizes.md);
the "parameter types not fixed" site (`Elaborator.Patterns.cs:87`) is expected to
dissolve under that ruling, not to need work of its own. Verified by the integrator
2026-09-24 at `32aa27e`.

## The program

```fun
{ M = module { pub pattern HasX(a) = struct { x: a; _ } }; 1 }
```

| runner | output |
|---|---|
| OCaml | `1` |
| port | `not ported yet: a pattern synonym over a type-case pattern` (`Elaborator.Patterns.cs:75`) |

(`pub` is required: a *block*-level `pattern` is the prototype's own recorded defect,
`pattern-synonym-not-a-block-declaration`.)

## Fix

The right-hand side elaborates; it is the wrapped pattern that needs the direct-match
machinery, so `core.NeedsDirectMatch()` is where the refusal is. Implement that path —
carry the core pattern into the sequential matcher — and remove the `:75` throw. The
settled ruling above changed what a synonym *is* (checked at its declaration, generalized
where its type is unknown), so read that ticket first and land this on top of it rather
than beside it: the two touch the same function.

Evidence for the `:87` half, so it is not re-probed: `{ M = module { pub pattern Head(a)
= Option.Some(a) }; 1 }` answers `1` in the prototype and refuses in the port with
"parameter types are not fixed" — it should disappear with the generalization.

## Tests

- The program above is an ordinary shared case (`expect` `1`; the prototype answers `1`,
  so no divergence entry).
- A companion that *uses* the synonym in a match, so the direct-match path is exercised
  rather than only elaborated, and a second one where the type-case inspects a
  sub-position.

## Reading

- `dotnet/src/Fun.Compiler/Elaborator.Patterns.cs:75` and `NeedsDirectMatch`
- [the pattern-synonym ruling](port-pattern-synonym-generalizes.md)
