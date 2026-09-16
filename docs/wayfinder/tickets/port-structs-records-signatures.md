---
title: "Port: structs, record construction and signatures"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
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
