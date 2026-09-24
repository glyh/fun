---
title: "Port: the reflected trait/impl forms with other than one parameter"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
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

## Ruling (integrator, 2026-09-25 — veto with one word if you disagree)

**One parameter is the rule.** Both implementations enforce it on every spelling a *user* can
write, and the port's AST encodes it. A macro-built reflected form that bypasses the check is
a **prototype defect** — the rule is real, that path simply skips it — so the convention-5
route applies: change nothing in the prototype, make the port's refusal the *language error*
its source path already raises, and **list the case in `prototype-divergences.txt`**.

That is the conversion this ticket's first version asked for, for the opposite reason, and
with a divergence entry the first version would not have added.

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
