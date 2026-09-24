---
title: "Port: the reflected trait/impl forms with other than one parameter"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
assignee:
blocked_by:
---

# Port: the reflected trait/impl forms with other than one parameter

**This ticket's first version was wrong, and a fork proved it by probing.** It said the arity
refusals were *parity* — the prototype rejecting the same spellings — on the strength of a
coverage sweep that had probed **source** spellings. At source level that is true. On the
**reflected** path it is false, and every site here is on the reflected path.

## What was measured (both runners, same source)

| site | shape probed | OCaml | port |
|---|---|---|---|
| `Reflection.cs:723` `RawTraitDef` | 0 / 2 / 3 params, body `Syntax.i64(1)` | **accepts → `1`** | `not ported yet: a trait with other than one parameter` |
| `:731` `RawImplDef` | 0 / 2 / 3 args | **accepts → `1`** | `not ported yet: an impl of other than one argument` |
| `:927` `DeclTrait` | 0 / 2 / 3 (the macro returns a `Decl`) | **accepts** | the same refusal |
| `:933` `DeclImpl` | 0 / 2 / 3 | **accepts** | the same refusal |
| `:767` `RawTypeDef` | a macro returning the reflected form | **accepts → `1`** | `not ported yet: reading the reflected form RawTypeDef` |

Controls: one-parameter macro-built forms pass in **both** runners. **Source-level** spellings
(`trait P = …`, `trait P(A, B)`, `impl P(I64, I64)`) are refused by **both** runners with the
same language error — in the port from `Enforest.Traits.cs:22-31,96-118`, an `ExpandException`
→ `FunException`, so no `NotImplementedException` on that path at all.

**Why the port refuses:** its AST hardcodes one parameter — `Syntax.TraitDef`/`Trait` hold a
single `Param` (`Syntax.Traits.cs:9,29`), `ImplDef`/`Impl` a single `Arg` (`:17,36`) — while the
prototype's carry `params`/`args` lists. So the port genuinely cannot *represent* another
arity: `NotImplementedException` is an honest marker, but it is not a decision.

## Ruled (user, 2026-09-25): **one parameter is the rule**

So the work is the conversion this ticket's first version asked for — for the opposite reason,
and with a divergence entry the first version would not have added: the prototype accepting a
macro-built reflected form with other arities is a **prototype defect** (its own source path
enforces the rule), and the port's refusal becomes the same language error that path raises.

## What to do

1. Convert the five sites to a `FunException` naming the rule (glossary vocabulary,
   convention 1). `:767` converts too — the fork probed it and it accepts like the rest.
2. Add one case: a macro building a two-parameter `RawTraitDef` (or `DeclTrait`), `expect`
   `error`, **listed as a divergence** naming this ticket. One is enough — the forms share
   the refusal.
3. Leave the source-level path alone: `Enforest.Traits.cs` already refuses those spellings.

## Reading

- `Reflection.cs:723`, `:731`, `:767`, `:927`, `:933`; `dotnet/src/Fun.Kernel/Syntax.Traits.cs:9,17,29,36`
- [the probed rows' conversions](port-probed-row-conversions.md) — the same shape of work
  (a refusal reclassified), done earlier

## Resolution (2026-09-25) — closed

Implemented on the user's ruling and merged (`e82b5d1`). All five sites now raise
`FunException`; the last `NotImplementedException` in `Reflection.cs` is gone, and the class
comment that claimed such forms are "not ported yet" is corrected with it.

- `:723` `RawTraitDef` → `trait declaration accepts exactly one parameter`;
  `:731` `RawImplDef` → `impl declaration accepts exactly one trait argument`; `:927`
  `DeclTrait` and `:933` `DeclImpl` the same two messages. The wording is byte-identical to
  what `Enforest.Traits.cs` raises for the source-level spellings, so both paths refuse alike.
- `:767` `RawTypeDef` → `type is a macro: the reflected syntax has no type definition` — **not
  an arity claim at all**, which is the right call: it follows the earlier closed ruling
  [do not port the reflected TypeDef](port-reflected-typedef.md), so the wording names the
  macro rather than a parameter count.
- Case `macros/reflected-trait-two-parameters`, `expect` `error`, **listed** in
  `prototype-divergences.txt` naming this ticket — 27 → **28**. A green OCaml run *with* the
  entry is the proof it is needed, since a listed case that passes is reported as a failure.
- **Verified by the integrator after merging:** C# 742 → **743 cases, 0 failed**; xUnit
  183/183; `dune test` and `dune test test/conformance` green — 743 cases, 0 failed, **28**
  divergences. No existing case changed behaviour, which is what the probe predicted.
- **Unverified:** `:927`, `:933` and `:767` share the checked refusal but no case exercises
  them — the ticket asked for one shared case, and the `RawTypeDef` shape has no shared angle.
