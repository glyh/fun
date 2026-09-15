---
title: Records are declared only by let bindings; recursion through `rec`
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-15
resolution: Implemented 2026-09-15 (branch recursive-records). A rec binding whose value is a struct type (under any parameters) mints a record identity; its body sees the name as a recursive occurrence (Core.RecOcc / VRecOcc) that unfolds on demand to the finished value and compares by identity. rec A = … and B = … groups (RecGroupBinding / LetRecGroup) hold struct types. type X = struct { … } and rewrite_record_self_refs are deleted.
decided: 2026-09-15
assignee:
blocked_by:
---

# Records are declared only by let bindings; recursion through `rec`

## Decision (grilled 2026-09-15)

One syntax per construct: a record type is a value, so it is declared only by a
let binding. `type X = struct { … }` is deleted, with an error naming the new form.

```fun
P = struct { x : I64 }                                           // plain record, structural
List = fn(A : Type) { struct { meta : A; count : I64 } }         // parameters are a function
rec Numbers = struct { head : I64; tail : Option(Numbers) }      // recursive
rec Tree = fn(A : Type) { struct { value : A; kids : List(Tree(A)) } }
```

- **A `rec` binding whose value is a struct type (possibly under `fn`) is where
  a recursive record gets its identity**, per
  [mutually-recursive-record-types](mutually-recursive-record-types.md) (identity
  at the knot). A plain let has no identity: structural.
- **Mutual records** are a `rec … and …` let group. There is no value-level
  `rec … and …` today (found by the rec-unfold-budget run); it has to exist for
  this, and the nominal three-phase knot moves onto it.

## Today (2026-09-15, probed on main)

| Written as | Result |
|---|---|
| `P = struct { x : I64 }` | works |
| `type P = struct { x : I64 }` | works (to be deleted) |
| `type List A = struct { meta : A; next : Option(List(A)) }` | declares; nesting a value fails (recursive-records-cannot-hold-a-record) |
| `type L = struct { meta : I64; next : Opt(L) }` (no params) | `UnboundVariable "L"` |
| `L = struct { …; next : Opt(L) }` | `UnboundVariable "L"` (a plain let does not see itself — correct) |
| `rec L = struct { …; next : Opt(L) }` | accepted; nesting a value gives `ApplyingNonFunction` |

`rewrite_record_self_refs` (`elab_syntax_util.ml`) matches the record by
`id.name` (an M12 survivor) and exists only for the `type` form; it goes.

## Work

- Delete `RecordTypeBinding` / `RecordTypeDef` (enforest, expand, reflection,
  elaborator's three copies) and migrate the prelude and tests.
- Implement with [self-type-has-no-identity](self-type-has-no-identity.md) and
  [recursive-records-cannot-hold-a-record](recursive-records-cannot-hold-a-record.md):
  the identity is minted by the `rec` binding.
- Coordinate with [adts-as-let-bindings](adts-as-let-bindings.md), which needs the
  same `rec … and …` group.
