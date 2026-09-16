---
title: Checking a value against an implicit function type inserts metas before binding
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Checking a value against an implicit function type inserts metas before binding

Found by the C# port's follow-up verification (2026-09-16). **To be fixed in the C#
port only**; the OCaml prototype keeps the defect.

## Decided (user, 2026-09-16): accept it

```
id = fn[A : Type](a : A) { a };
g : [A : Type] -> A -> A = id;
```

must check: `[A : Type] -> A -> A` is exactly `id`'s type. When the *expected* type
is an implicit function type, checking first binds its implicit parameter (as
checking a lambda does), then checks the value in that scope — where the value's
own implicit arguments are inserted against the body. Today both the prototype and
the port insert `id`'s implicit argument first and are left comparing a
non-implicit function with an implicit type (a function-type mismatch).

## Conformance

A shared case with `g`'s use succeeding, listed in
`test/conformance/prototype-divergences.txt` once the port passes it.
