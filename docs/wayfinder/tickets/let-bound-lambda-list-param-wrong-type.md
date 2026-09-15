---
title: Calling a let-bound lambda with a `List(I64)` parameter infers a wrong type
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-15
resolution: Fixed. `Nbe.eval_nominal` found a NomRef's template by scanning the environment for any VNominal with its id, and took the prelude's `Decls = List(Decl)` instance for the template, so `NomRef(List, [I64])` evaluated to `List(Decl, I64)`. It now matches only the unapplied template (no params). The prelude `expand_decls` annotation workaround is removed.
assignee:
blocked_by:
---

# Calling a let-bound lambda with a `List(I64)` parameter infers a wrong type

Found by the expand-decls run (2026-09-15); reproduced on main.

```fun
{ f = fn(d : List(I64)) { d };
  g = fn(d : List(I64)) { x : List(I64) = f(d); x };
  1 }
// error: UnifyError(NominalMismatch(List, List))
```

The inferred result of `f(d)` prints as `List(Decl, I64)`: an extra argument
(`Decl`) appears in the nominal's spine — likely a meta or implicit argument
leaking into a nominal application (an index/level shift, or a solved meta from
an earlier elaboration). A prelude wrapper was annotated to avoid it; remove that
workaround when fixed.
