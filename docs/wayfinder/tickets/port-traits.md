---
title: "Port: traits and impls"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: traits and impls

Wave 2 fork. Follow the porting conventions in
[port-core-tt-to-dotnet](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16).

## Scope

- `trait T(params) { field : type }`, `impl T(args) = { … }` and named
  `impl NAME : T(args) = …` in blocks, modules and structs (12 + 3 module items),
  trait dictionaries as values, evidence resolution at use sites, implicit
  dictionary insertion, trait bounds `[A : {Eq, Show}]` (1), `open` delivering
  impls (`OpenMember` for impls), signatures naming required impls.
- Deriving and protocols are parked design
  ([design-trait-library-deriving-and-protocols](design-trait-library-deriving-and-protocols.md),
  open): port what the prototype builds, stop at anything that ticket leaves open.

## Decided rules to read first

`docs/wayfinder/topics/impl-visibility.md` (if present), domain model I2 (impls
join the slot list), [dotted-paths-first-match](dotted-paths-first-match.md)
(named-impl lookups take the last). Implicit insertion lives in
`Elaborator.Implicits.cs`; module/struct entries in `Core.cs`/`Core.Structs.cs`.
