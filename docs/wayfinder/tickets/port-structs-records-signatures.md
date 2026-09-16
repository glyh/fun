---
title: "Port: structs, record construction and signatures"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
assignee:
blocked_by:
---

# Port: structs, record construction and signatures

Wave 1 fork. Follow the porting conventions in
[port-core-tt-to-dotnet](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16).

## Scope

- `struct { x : I64; y : Bool; pub f = … }`: a record type and a namespace in one
  (glossary **Struct**); field items and bindings in source order, a field's type
  seeing the items before it.
- Record construction `P{x = 1; y = 2}`, field access on a record, width
  subtyping where the prototype has it (`partial`), missing/extra/duplicate field
  errors.
- `sig { x : I64 }`: a signature value, checking a module against it
  (`(fn(m : sig { x : I64 }) { m.x })(module { pub x = 42 })`), unification of
  module types (today `Unify` raises "not ported yet: unifying module types").
- Methods inside structs only if a targeted case needs them and no prelude or
  effects dependency blocks it; otherwise stop at the boundary and report.

## Decided rules to read first

- Domain model I2 (slots), I3 (last member wins), I4b (members are not bare names).
- [struct open does not scope over `con_fields`](struct-open-does-not-scope-over-con-fields.md)
  (closed): the rule is settled there.
- Recursive records ([self-type-has-no-identity](self-type-has-no-identity.md),
  [recursive-records-cannot-hold-a-record](recursive-records-cannot-hold-a-record.md),
  [mutually-recursive-record-types](mutually-recursive-record-types.md)) are
  **out of scope**: they need `rec`, which another fork owns.

## Target

The conformance cases whose first blocker is "the `struct` form" (56) or "the
`sig` form" (18) and that need nothing else unported. Report which of them pass
and, for the rest, the next blocker.

## Other forks

match-enums, implicits, rec, imports run concurrently. Expect them in
`Syntax.cs`/`Core.cs`/`Enforest.cs`/`Expander.cs`/`Elaborator.cs`/`Nbe.cs`/`Unify.cs`
dispatch switches only.

## Resolution (2026-09-16)

Merged from `port/structs` (`d946001`, `6049d96`, merge `d6dad02`, `9536c8f`).
Structs, record construction and field access, signatures, module-type
unification (width subtyping), opening a signature-typed module, and struct
methods with `self`/`Self`. 28 cases newly pass, each error case checked for its
real message: values core-020, 022, 140, 142, 147, 150, 151, 167, elab-125,
imports/core-177; ok elab-078, 079, 117, 138, 139; error elab-086, 088, 090, 091,
093–095, 118, 119, 132, 133, 135–137. C# 60/610; xUnit 60.

Fixed on the way, both in slice 2a's C# code: run-time member access ignored
visibility (`module { pub x = 1; x = 2 }.x` gave 2), and `open m` with
`m : sig { … }` was rejected as a non-module.

**Follow-ups:**
- **Unverified prototype deviation (I3):** checking a module's type against a
  signature takes the *last* member of a name, per domain model I3; the prototype
  takes the first. Reproduce in OCaml; if confirmed, ticket it (see
  [dotted-paths-first-match](dotted-paths-first-match.md)), add a shared case and
  list it in `prototype-divergences.txt`.
- **Unverified prototype deviation (I2):** `nbe_quote.ml` reads a struct's
  bindings back all at `depth`, a module's at `depth + i`, though evaluation
  pushes one entry per binding in both. The port uses `width + i` for both.
  Reproduce and ticket as above.
- Not ported, marked so: method calls on a record (`v.m`, `self.m`), effect rows
  on methods, recursive records, impls and pattern synonyms in structs, `f{ e }`
  implicit application.
