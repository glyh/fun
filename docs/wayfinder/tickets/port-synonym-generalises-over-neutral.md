---
title: "Port: generalising a pattern synonym over a stuck neutral"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: generalising a pattern synonym over a stuck neutral

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
