---
title: "Port: implicit parameters and application"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
assignee:
blocked_by:
---

# Port: implicit parameters and application

Wave 1 fork. Follow the porting conventions in
[port-core-tt-to-dotnet](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16).

## Scope

- `fn[A : Type](a : A) { a }` and `[A : Type] -> …` types; explicit implicit
  application `f[I64]`.
- Implicit argument insertion: at application (`infer_ap`'s leading implicits), in
  checking's fallback (`wrap_implicits`), with inserted metas (`InsertedMeta`
  over the bound entries in scope).
- Solving a meta applied to a spine is already ported (the fixed renaming in
  `Unify.cs`); `elaborate/meta-solution-dependent-spine` must pass once implicit
  parameter lists read.
- Trait bounds `[A : {Eq}]` and trait dictionaries are **out of scope**: stop there.

## Target

Cases whose first blocker is "bracket expressions" (15), plus
`elaborate/meta-solution-dependent-spine`.

## Other forks

structs, match-enums, rec, imports run concurrently.

## Resolution (2026-09-16)

Merged from `port/implicits` (`35742a5`, merge `c333e98`). Implicit parameter
lists `fn[A : Type](a : A)` (explicit list optional), implicit arrow types
`[A : Type, B] -> …`, written implicit arguments `f[I64, Unit](1)`, and implicit
insertion before an explicit argument and in checking's fallback, over the bound
entries in scope. Trait bounds are "not ported yet". `elaborate/meta-solution-dependent-spine`
passes, verified to fail with the prototype's escape error when the renaming lift
is removed. New shared cases: `values/implicit-application` (7),
`values/implicit-inserted-when-checked` (3),
`elaborate/implicit-application-of-explicit-fn` (error). C# 31/610; xUnit 58.

**Follow-up (unverified):** checking a non-lambda against an implicit function
type inserts metas before unifying, as the prototype does, so
`g : [A : Type] -> A -> A = id` probably fails. No case exercises it. Reproduce in
the prototype and decide against the domain model before treating it as a defect.
Most remaining implicit cases (`core-072`…`084`, `elab-041`) are next blocked by
type-case `match`.
