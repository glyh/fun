---
title: Recursive records cannot hold a record
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Recursive records cannot hold a record

## Question

A self-recursive record type can be declared, and a value can be built whose
recursive field is empty. A record can never be placed into that field.

## Evidence

Found by the research section of
[mutually-recursive-record-types](mutually-recursive-record-types.md) (finding 1).

```
do type Opt A = Som A | Non; type L = {meta: I64; next: Opt(L)}
   l = L{meta = 1; next = Non}
   L{meta = 2; next = Som(l)} end                     → CannotUnify(Self vs struct type)
   … l.next end                                       → Non(Self) : Opt(Self)
```

Cause: `rewrite_record_self_refs` turns `L` in field types into `SelfType`,
which elaborates to a `VSelfType args` with no binder. Nothing ever unfolds it
back into the record type, and `unify.ml:480` only equates `VSelfType` with
another `VSelfType`. The existing tests (`test_elaborate.ml:468–479`) cover only
the declaration and the empty case.

## Direction

Depends on the knot decision in
[mutually-recursive-record-types](mutually-recursive-record-types.md): unfold an
identified `Self` on demand (option 1), or reject recursive record declarations
until that lands (option 4). Don't fix this without
[self-type-has-no-identity](self-type-has-no-identity.md): making `Self` unfold
while it still has no identity turns that bug into a live soundness hole.

Add the probes above as regression tests.
