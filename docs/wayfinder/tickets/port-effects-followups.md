---
title: "Port: effects follow-ups — ~> arrows, method result rows, modules that perform"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
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

## Resolution (2026-09-16)

Merged from `port/effects-followups` (`7d6d9d3`, `640d1cf`, `1e3007d`, merge `a34f58b`).
Method rows `->{E} T` / `->{_} T` sit on the innermost arrow, read with the
parameters bound; a method with no row is pure (E3). `~>` ported into
`Elaborator.PolyArrows.cs` (parameters mint row variables, the final arrow collects
them, an alias `Callback = Unit ~> I64` is rank 1, a written binder rank 2; checking
against an implicit row parameter binds it; `~>` outside a signature is an error).
A performing module makes its nominals generative and a performing member or `let`
gets a sealed type (`m1.make : I64 -> m1.T`), with escape checks. `Term.Map`
generalises `Shift`. Newly passing: alias-rank2-written, elab-223, 224, 226,
pure-colon-alias, alias-rank1-pure, elab-126, elab-165, 166; shared
`method-without-row-is-pure`, `generative-type-escapes-unnamed-module`,
`generative-type-escapes-its-binder` (agree with the prototype). C# 247/669; xUnit 123.

**Capture rule corrected:** a nominal captures only names that exist where its
enclosing module or body starts (the prototype's `enclosing_scope`); it had also
captured the module's own earlier members, which broke sealing.

**Not ported:** the run-time module stamp (a type-case head on a generative nominal,
generative type formers and generative rec-enum groups are "not ported yet").

**Open (user):** may a method infer its row with `method m() ~> T`? The prototype
rejects it (`PolyArrowOutsideSignature`) and so does the port.
