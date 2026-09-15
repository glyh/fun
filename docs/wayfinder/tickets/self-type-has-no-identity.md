---
title: Self type has no identity, so unrelated recursive records unify
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-15
resolution: Implemented 2026-09-15 (branch recursive-records). A rec binding whose value is a struct type (under any parameters) mints a record identity; its body sees the name as a recursive occurrence (Core.RecOcc / VRecOcc) that unfolds on demand to the finished value and compares by identity. rec A = … and B = … groups (RecGroupBinding / LetRecGroup) hold struct types. type X = struct { … } and rewrite_record_self_refs are deleted.
assignee:
blocked_by:
---

# Self type has no identity, so unrelated recursive records unify

## Question

`VSelfType` carries only type arguments, not the declaration it refers to. The
`Self` of one recursive record therefore unifies with the `Self` of any other.

## Evidence

Found by the research section of
[mutually-recursive-record-types](mutually-recursive-record-types.md) (finding 2).

```
do type Opt A = Som A | Non
   type L = {meta: I64; next: Opt(L)}
   type K = {name: String; next: Opt(K)}
   l = L{meta = 1; next = Non}
   K{name = "k"; next = l.next} end                   → accepted; should be rejected
```

Cause: `unify.ml:480` compares two `VSelfType`s by argument list alone
(`nbe_quote.ml:276` likewise).

Not exploitable today, because
[recursive-records-cannot-hold-a-record](recursive-records-cannot-hold-a-record.md)
keeps every such field empty. Fixing that bug alone would make this one live.

## Direction

Option 1 in the parent research: `VSelfType { id; args }`, minted per recursive
record declaration and compared by `id`. Under option 4 (reject recursive
records) this ticket closes along with the feature.

Add the probe above as a regression test that expects rejection.

## Decided (2026-09-15)

Identity at the knot, per [mutually-recursive-record-types](mutually-recursive-record-types.md): `Self` carries the recursive declaration's id and unfolds on demand. Implement the three tickets together.
