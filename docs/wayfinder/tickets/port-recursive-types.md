---
title: "Port: recursive type definitions"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: recursive type definitions

Wave 2 fork. Follow the porting conventions in
[port-core-tt-to-dotnet](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16).

## Scope

- `rec T = enum { … T … }`, parameterised `rec L = fn(A : Type) { enum { … L(A) … } }`,
  and mutually recursive groups `rec A = enum { … } and B = enum { … }` in blocks
  and modules (today "not ported yet: recursive type definitions", 14 cases).
- Recursive records: a `rec` struct type's reference to itself (glossary
  **RecursiveOccurrence**, compared by the identity its binding mints, unfolded on
  demand), records holding records, mutually recursive record types.
- **Fix a misclassification:** `values/core-144`
  (`Pair = fn[A, B] { struct {fst: A; snd: B} }; Pair{fst = 1; snd = True}`) fails
  with the *language* error "record construction of a non-struct". Constructing a
  record through a type former is unported, not ill-typed: raise
  `NotImplementedException` there, or port it.

## Decided rules to read first

[mutually-recursive-nominal-types](mutually-recursive-nominal-types.md),
[mutually-recursive-record-types](mutually-recursive-record-types.md),
[self-type-has-no-identity](self-type-has-no-identity.md),
[recursive-records-cannot-hold-a-record](recursive-records-cannot-hold-a-record.md),
[nominal-identity-applicative-by-purity](nominal-identity-applicative-by-purity.md);
domain model "What the port's types should be named after" (`RecursiveOccurrence`).
`Elaborator.Rec.cs` and `Elaborator.Enum.cs` are where rec values and enums live.
