---
title: "Port: implicit parameters and application"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
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
