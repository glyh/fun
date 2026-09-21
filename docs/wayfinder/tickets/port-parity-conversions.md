---
title: "Port: the 13 parity throws become language errors (convention 2)"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-20
resolution: 12 of the 13 rows were parity and now give the prototype's own language errors (17 unreachable catch-alls became invariant failures). The 13th was the audit's mistake - a dotted order-group path is a real gap, spun out to port-order-group-through-unit-path. C# 695 → 706 cases, 0 failed.
assignee:
blocked_by:
---

# Port: the parity throws become language errors

From [the unported-path audit](port-unported-path-audit.md), section "Parity: the
throw should become a `FunException`". Mechanical work, no design, but it is the
difference between an honest runner and a lying one.

## Why

[Porting convention 2](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16): an
unported form raises `NotImplementedException`; `FunException` is only for a genuine
language error, and **a missing feature must never make a case expecting `error`
pass**. Each site below is a **parity** row: the prototype refuses that form too, so
the port's refusal is correct — but it is raised as the wrong *kind* of refusal, so
an `expect error` case fails in C# instead of passing.

The live example, reproduced end to end by the integrator (2026-09-20):

```fun
{ 1 + 2 ~> 3 }
```

The prototype parses `(1 + 2) ~> 3` and type-errors; the port answers
`not ported yet: the polymorphic arrow ~>` because
`dotnet/src/Fun.Expand/Enforest.Roles.cs:148` throws **before** its `continues` guard.
A case expecting `error` fails in C#, not for lack of coverage but because the refusal
is the wrong kind. Fix the guard order here at least.

## The sites, with the prototype's refutation

| site | prototype |
|---|---|
| `Fun.Expand/Enforest.cs:220` | `enforest.ml:1445` |
| `Fun.Expand/Enforest.cs:291` | `enforest.ml:514` |
| `Fun.Expand/Enforest.cs:396` | `enforest_util.ml:342` |
| `Fun.Expand/Enforest.cs:412` (bare brackets) | `enforest.ml:87` |
| `Fun.Expand/Enforest.Roles.cs:148` (`~>`) | `enforest.ml:684` |
| `Fun.Expand/Enforest.Roles.cs:338` (dotted order group) | an open item on [brackets-decide-grouping](brackets-decide-grouping.md) |
| `Fun.Expand/Enforest.Traits.cs:143` | `enforest.ml:407` |
| `Fun.Compiler/Elaborator.cs:506` | `elab_resolve.ml:387` |
| `Fun.Compiler/Elaborator.Export.cs:52` | `elab_infer.ml:150` |
| `Fun.Compiler/Elaborator.Patterns.cs:47` | `elab_patterns.ml:187` |
| `Fun.Compiler/Elaborator.Structs.cs:80` | `enforest.ml:1509` |
| `Fun.Compiler/Elaborator.Structs.cs:209` | `elab_infer.ml:936` |
| `Fun.Compiler/Elaborator.Structs.cs:244` | `elab_type_expr.ml:74` |

Two of the prototype references are themselves open decisions in this repo
(`brackets-decide-grouping`), so for those the port must produce *some* language
error; which one is a detail, not a ruling.

## Also in this ticket

The audit's other two classes, which are cheap and belong with it:

- **Unreachable (17 sites)** — delete the lying catch-all. Convention 2 again: a
  catch-all that claims "not ported yet" about a path no program reaches misleads the
  next reader about what exists.
- **`Nbe.StuckMatch.cs:12`** — the one stopgap the audit called a real divergence
  rather than a shared choice: only an unknown *scrutinee* waits, where a known
  scrutinee with an unknown *part* should also wait (the prototype's rule). It is
  covered, with its undecided half, by
  [the latent form gaps](port-latent-form-gaps.md#g6-a-stuck-match-on-a-known-scrutinees-unknown-part).

## Tests

Each converted site that a program can reach gets a shared `expect error` case — that
is the point of the change, and the reason the count will move *up* rather than
holding. Verify each new case fails in the prototype before listing it in
`test/conformance/prototype-divergences.txt`; a case that passes in both is a normal
case, not a divergence.

## Resolution (2026-09-20)

Merged from `port/parity-refusals` (`d1d10fe`, commits `a9c8f12`, `7f45f83`).
**C# conformance 695 → 706 cases, 0 failed**; xUnit 172/172 unchanged; `dune test` and
`dune test test/conformance` green (706 cases, 20 divergences).

### The refusals now say what the prototype says

| site | now | prototype |
|---|---|---|
| `Enforest.cs:220` | `ExpandException("unsupported module item: …")` | `enforest.ml:1445` |
| `Enforest.cs:291` | `ExpandException("unsupported Phase 7A keyword: …")` | `:514` |
| `Enforest.cs:396` | leaves the unnameable operator unconsumed so `EnsureNoRest` reports it | `enforest_util.ml:342` |
| `Enforest.cs:412` + the **live** `Enforest.Implicits.cs` bracket path | `ExpandException("bare bracket expression is not in Phase 7A")` | `enforest.ml:87` |
| `Enforest.Roles.cs:148` | `ExpandException("not an infix operator: ~>")`, **`continues` guard moved ahead of the throw** | `enforest.ml:684` |
| `Enforest.Traits.cs:143` | `ExpandException("an impl in a signature must be named…")` | `:407` |
| `Elaborator.cs:510` | `FunException("open of a non-module")` | `elab_resolve.ml:387` |
| `Elaborator.Export.cs:63` | `"export of a non-module"` | `elab_infer.ml:150` |
| `Elaborator.Patterns.cs:47` | `"record pattern fields must follow a struct"` | `elab_patterns.ml:187` |
| `Elaborator.Structs.cs:80`, `:209`, `:244` | `unsupported struct item` / `record construction of a non-struct` / `unsupported signature item` | `enforest.ml:1509`, `elab_infer.ml:936`, `elab_type_expr.ml:74` |

**11 shared cases added**, all `expect error`, all pass in both runners, **none a
divergence**: `poly-arrow-is-not-an-infix-operator`, `module-item-that-is-not-a-binding`,
`keyword-in-expression-position`, `unnamed-impl-in-signature`,
`open-value-of-unknown-type`, `export-value-of-unknown-type`,
`record-pattern-head-of-unknown-type`, `struct-item-that-is-not-a-binding`,
`record-construction-of-a-non-struct`, `bare-bracket-expression`,
`pub-impl-is-not-an-expression`. `ExpandTests.RejectsUnported` folded into `Rejects`.

**Two rows are dead**, so their conversions are for the reader, not for a program:
`Enforest.cs:412`'s branch is unreachable because `ParsePrimary` intercepts bracket
groups first (the live path is `Enforest.Implicits.cs`, now converted too), and
`Elaborator.Structs.cs:244` is reachable only if a macro reflects a `RawSig` whose impl
carries fields — the enforester emits only `Let`/`Impl{Fields: null}`.

### The one row the audit got wrong

`Enforest.Roles.cs:343` (audit `:338`) is **not parity — it is a real gap**, and the
audit's reasoning is the part that failed: it read the site as
[brackets-decide-grouping](brackets-decide-grouping.md)'s open "dotted group
references" item, but the prototype resolves a dotted path of depth > 1 through a
module member. **Probed by the fork and re-verified by the integrator**:
`{ W = import "wrapper"; infix (@@) W.M.g ($x, $y) { $x }; 1 @@ 2 }` (units: `m` declares
`pub order g`, `wrapper` does `pub M = import "m"`) → the prototype answers `1`, the
port refuses. The fork left it alone and committed no case, which is right.
→ [an order group named through a unit member's path](port-order-group-through-unit-path.md)

The generalisable lesson: **"the prototype has an open ticket nearby" is not evidence
that a particular form is unported. Probe it.** The audit's correction is recorded in
its own parity table.

### Scope C

A `pub impl` parsed as an *expression* (a top-level program) refused the `pub` form; the
prototype gives `unsupported Phase 7A keyword: pub`, so it rides the `:291` conversion,
and the case `pub-impl-is-not-an-expression` pins it. At true unit level
(`ParseUnit` → `Module`) `pub impl` already worked in both.

### Integrator follow-up: invariant failures report, they do not abort

The 17 new `InvalidOperationException`s were uncaught by the conformance runner, so a
row wrongly believed unreachable would have **crashed the run** instead of failing one
case — hiding the other 705 results. The runner now catches
`InvalidOperationException`/`IndexOutOfRangeException`/`ArgumentException` per case and
reports `invariant failure (<type>): <message>`. An `expect error` case still does
**not** pass on one, so convention 2 is intact and the failure is simply visible.
