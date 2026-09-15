---
title: Small follow-ups from the 2026-09-15 runs
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Small follow-ups from the 2026-09-15 runs

1. **`self.a(2)` inside a struct fails at run time** with "field not found"
   (pre-existing; `a(self)(2)` works). Found by method-rows.
2. **A method's `can {E(k)}` row is expanded in the struct's scope**, not the
   parameters' scope, so it cannot name a parameter. Found by method-rows.
3. **A syntax form's `$(d : Decl)` hole must match the parameter kind** (M9: a
   capture `$(x : T)` is the parameter `(x : T)`): `Decl` captures exactly one
   declaration in a `{ … }` group, `List(Decl)` a group of any number. Today the
   hole kind `Decl` still reads the group as a list (decl-param-one left it).
4. **`Eq + Show` trait-bound sugar is recognised by the written name `+`**
   (`elab_syntax_util.trait_bound_forms`) — an M12 survivor; resolve with the `*`
   decision in [one-grammar-for-types](one-grammar-for-types.md).
