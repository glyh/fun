---
title: The coverage gaps the mutation sweep found
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by: []
---

# The coverage gaps the mutation sweep found

Found 2026-09-28 by the wide mutation sweep recorded in
[the suite redundancy investigation](suite-redundancy-measured.md). The sweep's main answer was
negative — no deletion list is defensible — but **32 of its 70 mutations caught nothing**, and that
is a coverage finding rather than a suite-health one. Artifacts: `/tmp/mut/table.tsv` and
`/tmp/mut/fails/<idx>-<tag>.txt` (may be gone; the table below is the record).

## The 32, split by what the sweep thought they were

**Look like genuinely untested behaviour** (the ones worth cases):

| mutation | area | what is untested |
|---|---|---|
| `ref-budget-limit` | `Budget.cs` | the evaluator's **limit** check — no conformance case reaches it. (Budget *errors* are cased as `rec-divergent-*`; this is the specific limit path.) |
| `ref-refs-nonreference` | `Elaborator.Refs.cs` | a reference to something that is not a reference |
| `ref-expander-macro-provisional`, `ref-macro-position`, `ref-macro-arity` | `Expander.Macros.cs` | a macro used before it is complete, in the wrong position, or at the wrong arity |
| `ref-enforest-empty-block`, `ref-enforest-block-export` | the enforester | an empty block, and an `export` in a block |
| `nbe-effects-row-dedup` | `Nbe.Effects.cs` | a duplicate effect surviving row normalisation |
| `nbe-generative-decl-match`, `nbe-traits-op-index` | `Nbe.*` | two same-shape distinct declarers; a trait operation index |
| `ref-pattern-syn-arity`, `ref-type-arity`, `ref-tuple-arity`, `ref-tuple-negative` | reflection | pattern-synonym, type, tuple and negative-arity helpers — only *type-parameter over-supply* has a case |
| `export-last-member`, `syntaxmap-operator-scope`, `expander-roles-attaches` | expander | three unexercised paths |
| `ref-nbe-continuation-used` | `Nbe.cs` | reading back a used continuation |

**Look redundant with another check** — a *code* question, not a test one: `ref-effects-poly-arrow`,
`ref-effects-nonexhaustive`, `ref-selopen-unknown-member`, `ref-export-clash`,
`ref-elaborator-tuple-len`, `ref-traits-implhead-field`, `ref-dup-member`, `ref-unknown-method`,
`ref-dup-bound`, `ref-missing-impl` (each answers to a sibling check that already refuses the same
input, so a case could not tell them apart).

**Too small to decide a case** (leave them): `mc-fieldscover`, `mc-pin-covers`,
`mc-universe-covers` — the structural `Covers` relation never decides a case on its own.

## What a fork should do first

Five to six cases, in this order, all `error`-expecting and all in
`test/conformance/cases/elaborate/` or `cases/values/` as the subject fits:

1. `ref-budget-limit` — the limit path, which nothing reaches.
2. `ref-refs-nonreference` — the ref refusal.
3. `ref-macro-arity` and `ref-macro-position` — user-visible macro diagnostics.
4. `ref-enforest-empty-block` — an enforester refusal, cheap to write.
5. `nbe-effects-row-dedup` — an evaluator path, if a program can reach it.

**Every case must be proven to catch something**: re-apply the mutation it was written for and show
the new case fails, then revert. A case that pins a refusal the mutation cannot remove is worth less
than the mutation that motivated it, and this ticket exists because that went unmeasured for 848
cases.

## What this ticket is not

- **Not a deletion list.** The sweep's zero-catch counts are a statement about its own 19 flips, and
  [the investigation](suite-redundancy-measured.md) explains why they cannot convict a case.
- **Not message-pinning.** A conformance `.expect` is a value or the literal `error`; the exact
  message belongs in xUnit (`CLAUDE.md`). These cases assert *that* the refusal happens — which is
  precisely what the mutations showed was unasserted.
- **Not exhaustive.** The sweep could not mutate `Driver`, `Loader`, `Reflection`, `PreludeAbi`, the
  `Core.*` traversals, deeper `Unify`, `Nbe.Structs`/`Nbe.Rec`, `Expander.Imports`, `Reader` beyond a
  caret flip, or `Syntax.Map` beyond one flip, so their cases are unmeasured rather than safe.
