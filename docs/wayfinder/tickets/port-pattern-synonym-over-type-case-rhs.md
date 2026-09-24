---
title: "Port: a pattern synonym over a type-case pattern"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
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

## Resolution (2026-09-25) — closed

Merged from `pi-agent-0b6cda9e-c3d1-4e5` (`dc57698`). The blanket refusal was **narrowed**, not
removed: a `StructType` pattern carries no head term, so `SelectArmInOrder`/`Matches` and
`FillSynonymParams` already handled it — the only blocker was `core.NeedsDirectMatch()`
covering `StructType` and `NominalHead` together. It now refuses only a **nominal** type-case
head (`ContainsNominalHead`), the half that needs a definition-site closure to carry its head
term.

- Cases: `pattern-synonym-over-struct-type-case`, its `-use` companion (which *runs* the
direct-match path rather than only elaborating) and `-subposition`, all ordinary (`expect`
`1`; the prototype answers `1`). Plus `pattern-synonym-over-option-constructor`, pinning this
ticket's other half: `{ M = module { pub pattern Head(a) = Option.Some(a) }; 1 }` used to
refuse with "parameter types not fixed" and now answers `1` — dissolved by the generalization
work, as predicted.
- **Verified by the integrator after merging:** C# 743 → **747 cases, 0 failed**; xUnit
  183/183; `dune test` and `dune test test/conformance` green — 747 cases, 0 failed, **28**
  divergences.
- **The sketch did not survive, correctly.** The dead fork's shape (`NominalHead.Head`:
  `Term` → `Value`, plus removing the runtime-env re-evaluation in `Nbe.Generative.cs`) was
  rejected for a concrete reason: the stored value is already `Option(?m)`, so
  `MatchesNominalHead` would apply fresh metas a second time, and the dropped re-evaluation is
  exactly what E11's sealed projections need. None of `Core.Patterns.cs`, `Nbe.Generative.cs`,
  `Nbe.Patterns.cs` or `Elaborator.Match.cs` was touched, `NominalHead.Head` stays `Term`, and
  every `values/nominal-*` and `elab-067` identity case passes unchanged. The sketch branch is
  deleted.
- **Residue, verified by the integrator and spun out:** a zero-argument **sealed-nominal** head
  still refuses while the prototype accepts it →
  [a pattern synonym over a sealed-nominal head](port-pattern-synonym-over-sealed-nominal-head.md).
  A *former* head is parity, not a gap — the prototype answers `UnknownConstructor "Option"`
  for `pub pattern IsOpt(a) = Option(a)`.

## First attempt (2026-09-24) — died on a provider limit; WIP preserved as a sketch

The fork was killed by the provider's 5-hour cap (resets 2026-09-25 01:35:34) after 59 tool
calls, mid-diagnosis. **Nothing was merged, nothing is verified, and the ticket stays open.**

Its own last words pin the crux: `TypeHead`'s value is already applied to arity metas
(`Option(?m)`, a `VNominal`) while `MatchesNominalHead` needs the *unapplied former* — and
for direct arms the head term is (re)evaluated under the runtime environment, which matters
for E11 sealed projections. It was heading into the E11 cases when it died. That first
observation is a real code fact, and it is why the change reaches into `NominalHead` at all.

What it left is on branch `pi-agent-64a8991f-7d64-4ad` (commit `29f4852`, auto-squashed).
**Treat it as a sketch of an approach, not as workable code:**

- the `NeedsDirectMatch()` refusal is gone, with a comment saying the right-hand side is
  carried into the synonym and runs where the synonym is used (the match's `Sequential`
  tree, `Nbe.SelectArmInOrder`);
- `CorePattern.NominalHead` changes shape — `Term Head` → `Value Head` — so a pattern stored
  in a synonym's right-hand side does not carry a term from its definition's context;
- `FillSynonymParams` gains a `NominalHead` case, and `Nbe.Generative.cs` stops
  re-evaluating `head.Head` under the runtime environment.

**The last point is the one to be suspicious of**, and it is why this must be re-derived
rather than merged: the same fork had just observed that direct arms *do* re-evaluate the
head under the runtime env for E11 sealed projections, so a Kernel shape change that removes
that re-evaluation has to be run against the identity cases (`values/nominal-*` and the
generative ones) before it can be believed. None of that was done.
