---
title: "Port: traits and impls"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
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

## Resolution (2026-09-16)

Merged from `port/traits` (`11f93d4`, `e63d83a`, `6e8ffde`). `trait T(A) = sig { … }`
in blocks and modules; anonymous and named impls in blocks, modules and structs;
`name : impl T(A)` signature members. Evidence resolves from lexical scope (an impl
for another argument does not count; two different impls are an ambiguity error;
the same module opened twice brings its impl once). Bounds `[A : Eq]` /
`[A : {Eq, Show}]` add one hidden dictionary each, inserted at application and
resolved after the explicit argument is checked; `Trait.op` calls; `open`
delivers public impls; impls contribute only through the slot list (I2) and named
lookup takes the last (I3). Newly passing: `elab-026` (7), `elab-197` (error),
and new shared cases `values/trait-bounded-call` (8),
`values/trait-impl-through-open` (4), `elaborate/trait-impl-needs-open` (error),
`elaborate/trait-impls-ambiguous` (error), each agreeing with the prototype; the
other 15 trait cases now stop at the prelude. C# 109/636; xUnit 79.

**Follow-ups:**
- `Trait.op` takes the innermost impl of the trait whatever its argument, as the
  prototype does (`ponytail:` in the code). Revisit against the traits design.
- Re-exporting impls through `export` is not ported (`core-312`, `core-313`, which
  are first blocked on the prelude's `==` and `if`).
