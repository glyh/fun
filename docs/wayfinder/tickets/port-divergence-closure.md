---
title: "Port: close the recorded divergences"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-20
resolution: 13 of the 14 tickets named by prototype-divergences.txt are closed; trait-op-takes-innermost-impl is deliberately left open (its grilled generic-impl ruling is unimplemented and owned by step 2). All 19 listed cases re-verified to fail in the prototype and match their .expect in C#.
assignee:
blocked_by:
---

# Port: close the recorded divergences

Step 4 of [reach feature parity with the prototype](port-parity-plan.md). The work
is verification and bookkeeping — no feature code. Every case in
`test/conformance/prototype-divergences.txt` is already correct in C#: the file
says so, and the C# runner passes all of them. The file's **19 case lines** name
**14 tickets**, and every one was still `status: open` because the port being
correct was recorded in the bodies but never flipped the front-matter, leaving
the map's frontier wrong about 14 tickets.

## Verification (2026-09-20, `main @ d58af64`)

`prototype-divergences.txt` is a two-sided contract — the OCaml runner reports a
listed case that *passes*, the C# runner compares every case to its `.expect` —
so running both validates the file against reality:

- **OCaml** — `dune test --root . test/conformance` →
  `690 cases, 0 failed, 19 known prototype divergences`. Every listed case fails
  as expected; none passes (which the runner would report as "passes, but
  prototype-divergences.txt lists it").
- **C#** — `cd dotnet && dotnet build -v q --nologo && dotnet run --project
  test/Fun.Conformance --no-build` → `690 cases, 13 failed`. Every one of the 19
  listed cases therefore matches its `.expect`; the 13 failures are the unrelated
  stage-2 residue (`method calls on a record` ×4, `Eq` impl resolution ×2,
  `elab-059`, `elab-062`, `elab-067`, `core-067`, `core-102`, `core-270`,
  `imports/core-165`) and **none is listed in the divergence file**.

No line was removed from or added to `prototype-divergences.txt`: every listed
case still diverges, so the file's assertion stands unchanged.

## Verdicts

| Ticket | Case lines | Verdict |
|---|---|---|
| [lambda-check-ignores-written-parameter-type](lambda-check-ignores-written-parameter-type.md) | `elaborate/elab-049`, `elaborate/lambda-param-type-mismatch` | closed — fixed in the port only |
| [meta-solution-renaming-not-lifted-under-binders](meta-solution-renaming-not-lifted-under-binders.md) | `elaborate/meta-solution-dependent-spine` | closed — fixed in the port only |
| [bare-constructor-pattern-resolves-by-name](bare-constructor-pattern-resolves-by-name.md) | `elaborate/bare-constructor-pattern-needs-open`, `…-shadowed-by-value` | closed — fixed in the port only |
| [recursive-record-field-of-own-type-rejected](recursive-record-field-of-own-type-rejected.md) | `values/rec-record-field-of-own-type` | closed — fixed in the port only |
| [type-case-former-head-arity-from-template](type-case-former-head-arity-from-template.md) | `values/type-case-former-parameter` | closed — fixed in the port only |
| [pattern-synonym-arguments-bind-by-position](pattern-synonym-arguments-bind-by-position.md) | `values/pattern-synonym-binds-by-name`, `…-through-open` | closed — fixed in the port only |
| [pattern-synonym-not-a-block-declaration](pattern-synonym-not-a-block-declaration.md) | `values/pattern-synonym-in-block` | closed — decided 2026-09-17, port accepts it |
| [signature-check-takes-first-member](signature-check-takes-first-member.md) | `values/signature-check-takes-last-member` | closed — fixed in the port only |
| [check-against-implicit-type-inserts-first](check-against-implicit-type-inserts-first.md) | `values/check-against-implicit-type` | closed — decided 2026-09-16, port now passes |
| [panic-with-unknown-message-fails-checking](panic-with-unknown-message-fails-checking.md) | `values/panic-unknown-message-in-type` | closed — port already correct |
| [deref-of-unknown-type-is-not-a-reference](deref-of-unknown-type-is-not-a-reference.md) | `values/deref-infers-reference` | closed — port already correct |
| [method-cannot-infer-row-with-poly-arrow](method-cannot-infer-row-with-poly-arrow.md) | `values/method-poly-arrow-infers-row` | closed — fixed in the port only |
| [unit-handle-open-not-a-unit-open](unit-handle-open-not-a-unit-open.md) | `imports/unit-handle-open-form-member` | closed — fixed in the port only |
| [trait-op-takes-innermost-impl](trait-op-takes-innermost-impl.md) | `values/trait-op-resolves-by-argument`, `values/trait-impl-per-argument`, `elaborate/trait-op-nearness-no-tiebreak` | **left open — excluded** |

## Not closed

**`trait-op-takes-innermost-impl` stays `open`.** Its precision-order half was
fixed in the port (2026-09-17), but the ticket also carries the grilled ruling of
2026-09-18 — a free name in an impl's head binds implicitly, with no declaration —
which is **implemented nowhere yet** (neither prototype nor port). That is real
work owned by step 2 of the parity plan, not bookkeeping, so it is deliberately
excluded from this closure pass.

## Note: one body was stale about its own state

[check-against-implicit-type-inserts-first](check-against-implicit-type-inserts-first.md)
still read "**To be fixed** in the C# port only" and "Today both the prototype and
the port insert `id`'s implicit argument first", but the port already passes the
case. It was closable because the body records the *decision* (accept it), and the
closure resolution notes that the "Today both …" paragraph is the pre-fix state.
No other ticket in the directory needed this: the rest either record the fix in
the past tense or are legitimately unresolved.
