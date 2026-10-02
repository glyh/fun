---
title: Enforester improvements scope
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee:
blocked_by:
---

# Enforester improvements scope

> **Unblocked 2026-09-26** — `design-type-aware-macro-interleaving.md` closed, so the blocking
> edge was stale. Re-framed the same day: this ticket was written to scope work *in the OCaml
> prototype before the C# rewrite*, and the prototype was deleted 2026-09-25 while the port became
> the implementation. The question survives; its targets do not.

## Question

Which reader / enforester improvements are worth doing **now** — with `src/Fun.Expand` the only
implementation, no rewrite coming to make the effort disposable, and the surface still moving?

## Context

- [enforester-improvements](../topics/enforester-improvements.md) describes two phases:
  1. Structured errors and fault-tolerant parsing (spans, recovery, incremental).
  2. Spec-oriented structure (declarative combinators, generic driver).

  **That topic is written against the deleted prototype** — every path and code block in it is
  `enforest*.ml`. It is an *input* to this question, not an inventory of the port; re-deriving it
  against `src/Fun.Expand/Reader.cs`, `Enforest*.cs` and `Expander*.cs` is part of the answer,
  and nothing measured today backs any of the two phases' claims.
- The old roadmap cautioned against broad diagnostics cleanup pre-rewrite. The rewrite has
  happened, so that constraint is void — but "one implementation, surface still moving" is not
  the same as "polish now". That boundary is what this ticket decides.
- Work that already points at the enforester from elsewhere:
  [Stage 12 diagnostics](specify-stage-12-macro-diagnostics-and-expansion-ux.md) (there is no
  non-fatal diagnostic channel), [the runner does not timebox elaboration](port-runner-does-not-timebox-elaboration.md)
  (a reader that stops advancing must fail the suite, not hang it), and
  [brackets decide grouping](brackets-decide-grouping.md) (open hole-extent questions).

## Re-derivation (2026-10-01, measured against `src/Fun.Expand`)

Every path and code block in [enforester-improvements](../topics/enforester-improvements.md) is
`enforest*.ml`; **none** of the prototype machinery it names exists here. What the port has is
`Reader` (scan → delimiter pairs, whole source, no recovery) → `Enforest` (statement/expression
readers over `TokenTree`) → `Expander` (per statement, scope/role/macro). Two facts frame the
ruling: every token already carries a `SourceSpan` (`Reader.cs:74`), and **no expansion error
carries one** — three flat, message-only exception types (`Reader.cs:9`, `BinderTable.cs:21`,
`:28`) are all re-wrapped as `FunException(e.Message)` at `Driver.cs:38-46`, so a span is
structurally unreachable at the edge even where it exists upstream.

Numbers measured this round, each reproducible: `wc -l src/Fun.Expand/Enforest*.cs` = **2804**;
`grep -rn 'throw new' src/Fun.Expand/*.cs | wc -l` = **173**, none carrying a span;
`grep -niE 'spec|combinator|pratt' src/Fun.Expand/*.cs` = **0 hits**;
`grep -n '\[\], *span' src/Fun.Expand/Enforest*.cs` = **0**.

| topic item | port equivalent | status |
|---|---|---|
| Phase 1 structured `Parse_error` (`kind` + `span`) | none — the three string-only types above | **remains** (spans exist upstream, discarded at `Driver.cs:38-46`) |
| Phase 1 error accumulator | none (no `errors` field) | **unmeasurable as stated** — `.expect` has no multi-error form (`cases/README.md:20-27`) and no reader-failure corpus exists |
| Phase 1 recovery (`skip_to_statement_boundary`, `skip_to_close`) | none; only the non-advance guard `Enforest.RequireAdvance` (`Enforest.cs:585`) | **remains, unmeasured** — the workload (how many cases fail inside `Fun.Expand`) has never been counted |
| Phase 1 precise expected/got wording | wording is pinned in xUnit (`test/Fun.Tests/ReaderTests.cs:65`) | **unmeasurable as stated** — it depended on Phase 2's spec, which is gone |
| Phase 1 incremental parsing | `Reader.Read` takes a whole `string` (`Reader.cs:35`) | **dropped** — no incremental surface to name |
| Phase 2 combinator library + generic driver | none | **remains as a question, not a plan** — a proposal needs a consumer first |
| Phase 2 Pratt driver for `parse_expr_prec` | replaced by role-driven named order groups (`Enforest.cs:244`, `Enforest.Roles.cs:80`, `:150`, `:429`) | **dropped** (changed shape) |
| Phase 2 unconsumed-terms / `parse_fn_parts` rest bug | `EnsureNoRest` refuses a leftover rather than returning one (`Enforest.cs:118`) | **done** |
| Stage 12's "no non-fatal diagnostic channel" | still true — the only `warning` hits in `src/` are two comments | **remains** (a count, decidable by reading) |

**Forkable now, in this order** (both are buildable, neither needs a ruling):

1. **Carry the span on expansion errors.** Acceptance: each of the three exception types carries a
   `SourceSpan`, `FunException` can hold one, `Driver.cs:38-46` stops discarding it, and the
   wording assertions in `ReaderTests.cs:65` are updated to include it. Independent of the
   ruling below, and it is the same work the fog item "elaborator errors carry no source location"
   needs.
2. **An error-site inventory and taxonomy** to replace the topic's `[x]` checkboxes — a
   grep-derived table, not a plan.

**Not forkable yet, and this is the ruling's real content:** the accumulator and recovery phase has
**no demonstrated consumer**. `test/conformance/cases/README.md` cannot express two errors from one
program. **The probe was run 2026-10-01** (instrumenting `Driver.cs`'s three catches with a marker and
probing all 238 `error` cases through `--file`, then reverting): **20 of 238 are refused inside
`Fun.Expand`**, in this shape:

| category | cases |
|---|---|
| order-group refusals | 4 (`order-group-missing`, `order-through-binder`, `order-assoc-none-does-not-chain`, `order-groups-unrelated`) |
| macro refusals | 3 (`macro-arity`, `macro-position`, `non-trailing-hole-matches-one-term`) |
| "unsupported Phase 7A" enforester gaps | 3 (`keyword-in-expression-position`, `pub-impl-is-not-an-expression`, `bare-bracket-expression`) |
| role/binder and module-item shapes | 5 |
| pattern/term shapes and the `struct-field-*` spellings | 5 |

**What the count settles and what it does not.** It settles that expander refusals are a real category
(twenty cases pin them) and that the first-error path is exercised. It does **not** show a consumer for
*recovery*: every one of those programs has exactly one error, so nothing asks for a second message
from the same program. The decision is now the user's, made against this number rather than against
the prototype's topic doc.

**A finding the same probe produced, which the map's fog had half right.** `struct-field-comma` and
`-trailing-comma` already print a position: `unexpected token in a definition context at
<unknown>:2:22-2:23`. Expansion errors *can* carry a span in their text; what `Driver.cs:38-46`
discards is the structured span, not the text — so anyone adding positions on the expansion side
should start from what the messages already say.

## Resolution

**Ruled 2026-10-01:** error recovery is **out of scope** — the workload was counted (20 of 238 `error`
cases are refused inside `Fun.Expand`, every one of them single-error) and no program asks for a second
message, which is the only thing recovery buys. The remaining half stays live: **carrying the span on
expansion errors**, whose starting point is that `struct-field-comma` and `-trailing-comma` already
print `at <unknown>:2:22-2:23`, so what `Driver.cs:38-46` discards is the structured span rather than
the text.

That makes this ticket a one-item ticket, and it is forkable as it stands: an expansion error's span
can be carried out through the driver's three catches (the same funnel the elaborator-side ruling
touched) without inventing recovery. The boundary is worth stating: expanding from the first error
and stopping is **decided**, not a backlog item — a later session should not re-open it without a
program that reports two.
