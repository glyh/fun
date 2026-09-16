---
title: "Port: effects follow-ups — ~> arrows, method result rows, modules that perform"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: effects follow-ups — `~>` arrows, method result rows, modules that perform

Wave 3 fork. Follow the porting conventions in
[port-core-tt-to-dotnet](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16).

## Scope

- **`~>` elaboration** (6 cases "not ported yet: ~> arrows"): every function type is
  read on its own; a parameter's type is a signature in its own right, and the
  chain's final arrow carries the row variables its parameters minted (the
  prototype's `Elab_poly_arrows`); on a definition the final arrow also infers what
  the body performs; a final `~>` with no parameter to collect from mints its own
  variable; a type that mints its own row (`Callback = Unit ~> I64`) is a function
  of that row, rank 1 at the definition taking it.
- **Rows on method results** (5 cases): `pub method m() ->{Exc} I64 { … }`; a method
  with no declared row is pure (E3).
- **Modules that perform** (generative nominals, E11): a module binding whose
  evaluation performs gets a fresh stamp per evaluation, so its types are distinct
  instances; add the module stamp slot through the slot list (`ponytail:` notes in
  `Elaborator.InferModule` and `Elaborator.Enum.cs`), and `check_sealed_*` rules.

## Decided rules to read first

`docs/wayfinder/topics/core-tt-domain-model-effects.md`,
[effect-arrow-syntax](effect-arrow-syntax.md), [methods-follow-the-arrow-rule](methods-follow-the-arrow-rule.md),
[nominal-identity-applicative-by-purity](nominal-identity-applicative-by-purity.md),
`STATUS.md` "Effects on the arrow; `~>`; bound sets"; the effects fork's merge
record in [port-effects](port-effects.md).
