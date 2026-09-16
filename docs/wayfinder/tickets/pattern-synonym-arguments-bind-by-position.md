---
title: Pattern synonym arguments bind by position, and a synonym is not found through open
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Pattern synonym arguments bind by position, and a synonym is not found through open

Found while porting pattern synonyms to C# (2026-09-16). **Fixed in the C# port
only**; the OCaml prototype keeps both defects.

## Decided (user, 2026-09-16)

A synonym's arguments bind **by parameter name**: a synonym means what its
definition says. Its head resolves like any other bare pattern head, through its
binder or an open
([bare-constructor-pattern-resolves-by-name](bare-constructor-pattern-resolves-by-name.md)).

## Defect 1: arguments substitute by position

```
Point = enum { Pt(I64, I64) };
open Point;
pattern Flip(a, b) = Pt(b, a);
match (Pt(10, 20)) { Flip(first, second) => first }
```

Should give 20 (`first` is `a`, in `Pt`'s second slot). The prototype substitutes
the use's sub-patterns into the right-hand side by position, giving 10, so the
parameter names in the declaration mean nothing.

## Defect 2: a synonym reached through `open` is unknown

A synonym declared in a module and used after `open M` fails with
`UnknownConstructor`.

## Conformance

Added with the port (names fixed here so the list matches):
- `values/pattern-synonym-binds-by-name` (20) - defect 1
- `values/pattern-synonym-through-open` - defect 2
- `values/pattern-synonym-agrees` - a use on which both readings agree

The two defect cases are listed in `test/conformance/prototype-divergences.txt`
once they land and are confirmed to fail in the prototype.
