---
title: A recursive record whose field has the record's own type cannot be used
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# A recursive record whose field has the record's own type cannot be used

Found by the C# port's recursive-types fork (2026-09-16). **Fixed in the C# port
only**; the OCaml prototype keeps the defect.

## Defect

```
{ rec L = struct { v : I64; next : L }; f = fn(l : L) { l.v }; 1 }
```

fails in the prototype with `UnifyError(CannotUnify(struct type vs Type))`. The
declaration alone is accepted, and so is `next : Option(L)`: only a field whose
type is the bare recursive occurrence breaks, once the record is used as a
parameter type. The closed
[recursive-records-cannot-hold-a-record](recursive-records-cannot-hold-a-record.md)
decides that a recursive occurrence *is* a type (glossary
**RecursiveOccurrence**), so the field is well-formed.

## Conformance

`values/rec-record-field-of-own-type` (1), listed in
`test/conformance/prototype-divergences.txt`. The port also pins it in
`dotnet/test/Fun.Tests/RecTypesTests.cs`.
