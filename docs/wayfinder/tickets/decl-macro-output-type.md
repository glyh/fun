---
title: A Decl macro's output is typed — `Decl` or `List(Decl)`
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-15
resolution: Implemented. `: Decl` / `: List(Decl)` is the type the body is checked against at the definition (`Syntax.macro_compiled`); `quote { … }` against `Syntax.Decl` must hold one non-hole item (`QuoteNotOneDecl`); the `VU` workaround is deleted.
decided: 2026-09-15
assignee:
blocked_by:
---

# A Decl macro's output is typed — `Decl` or `List(Decl)`

## Defect

A `: Decl` macro's body has no output type: returning one `Decl` and returning a
list both happen, so nothing checks either. Found by the decl-macro-arity run
(`macro m(_) : Decl { Nil }` returns an unapplied implicit function, papered over
by instantiating it with `VU`).

## Decision (grilled 2026-09-15)

The annotation says which, and the body is checked against it at the definition:

```fun
macro one() : Decl { quote { a = 1 } }                  // exactly one declaration
macro many() : List(Decl) { quote { a = 1; b = 2 } }    // any number (= Syntax.Decls)
```

- `quote { … }` fits either: checked against the expected type, it must hold
  exactly one item when a `Decl` is expected, any number for `List(Decl)`.
- A mismatch is an error at the definition (`: Decl` whose quote holds two items).
- Both splice at a call in item position; a `$d` hole keeps taking `List(Decl)`
  (a `Decl` value is not silently wrapped).
- Remove the `VU` instantiation workaround once the body is checked.
