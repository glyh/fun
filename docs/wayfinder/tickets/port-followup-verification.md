---
title: "Port: verify recorded deviations, fix core-311, runners evaluate error cases"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: verify recorded deviations, fix `core-311`, runners evaluate `error` cases

Wave 3 fork. Follow the porting conventions in
[port-core-tt-to-dotnet](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16).

## 1. Verify the unverified deviations

Each was recorded by a fork as a place where the port departs from the prototype,
but not reproduced. For each: write the smallest program, run it through the
prototype (temporary conformance case) and the port, and report one of: *agrees*
(no deviation after all), *prototype defect* (the domain model / glossary / a
closed ticket decides for the port — report the repro; the integrator tickets it),
or *undecided* (stop and report with the example).

- [structs](port-structs-records-signatures.md): checking a module against a
  signature takes the last member of a name (I3); the prototype the first.
- [structs](port-structs-records-signatures.md): `nbe_quote.ml` reads a struct's
  bindings back at `depth`, a module's at `depth + i`.
- [export](port-export.md): the constructor-named-like-its-enum exemption through
  an open.
- [implicits](port-implicit-parameters.md): `g : [A : Type] -> A -> A = id` —
  insertion before unification.
- [primitives](port-primitives.md): `panic` with an unknown message.
- [refs](port-refs.md): reading/writing a value of meta type.

## 2. Fix `macros/core-311`

"`A` is not a constructor in scope": constructors that arrive through `export` and
then `open` are not recorded as constructor entries, so a bare pattern head cannot
resolve to them (recorded on [recursive types](port-recursive-types.md)).

## 3. Both runners evaluate an `error` case that elaborates

`test/conformance/cases/README.md` defines `error` as failing "at expansion,
elaboration or evaluation", but `run_conformance.ml` and `Fun.Conformance` only
elaborate an `error` case. Make both run it when it elaborates, so a run-time
failure (overflow, division by zero, `panic`) counts. This edits the OCaml *test
runner*, not the prototype. Then add the run-time error cases the primitives fork
had to drop (overflow, division by zero) as shared cases. If a case expecting
`error` elaborates and then does not terminate, stop and report it.

## Resolution (2026-09-16)

Merged from `port/followup-verification` (`a9593ed`, `1bba77c`, `1d8578e`).

| # | Deviation | Verdict |
|---|---|---|
| 1 | signature check takes the last member | **prototype defect** (I3): [signature-check-takes-first-member](signature-check-takes-first-member.md), `values/signature-check-takes-last-member` listed |
| 2 | struct readback depth in `nbe_quote.ml` | **not observable**: no program reaches a struct binding's readback; closed |
| 3 | export exemption through `open` | **C# bug, fixed**: `N.T` is not a member of `M`, so it clashes; `elaborate/export-clash-through-open` (error) |
| 4 | `g : [A : Type] -> A -> A = id` | both reject it; **decided (user): accept** — [check-against-implicit-type-inserts-first](check-against-implicit-type-inserts-first.md) |
| 5 | `panic` with an unknown message | **decided (user): stays unevaluated** — [panic-with-unknown-message-fails-checking](panic-with-unknown-message-fails-checking.md) |
| 6 | `deref` on a value of unknown type | **undecided** (user) |

`core-311` fixed: module fields carry a `ConstructorMark`, so `OpenModule` restores a
constructor entry that arrived through `export`. Both runners now run an `error`
case that elaborates (the C# runner stops a run after 10 s; the OCaml runner has no
guard); no existing outcome changed. New shared cases `values/runtime-i64-overflow`,
`runtime-division-by-zero` (error), `runtime-arithmetic-in-range`. A lone operator in
parentheses reads as a name, as in the prototype. C# 236/666; xUnit 123; OCaml 666
cases, 0 failed, 10 known divergences.

**Open (user):** #6.
