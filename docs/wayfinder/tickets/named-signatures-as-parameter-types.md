---
title: A let-bound signature cannot be a parameter type; dependent signatures fail
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# A let-bound signature cannot be a parameter type; dependent signatures fail

Found by the decl-output-open-param run (2026-09-15); pre-existing.

```fun
Sig = sig { x : I64 };
f = fn(m : Sig) { m.x }          // CannotUnify(module value vs Type)
g = fn(m : sig { x : I64 }) { m.x }   // works (inline)

h = fn(m : sig { T : Type; v : T }) { m.v }   // fails, even without open
```

- A signature is a value (types are values), so a let-bound one must work
  wherever the inline form does.
- A dependent signature (a later member's type mentions an earlier type member)
  must elaborate.
- Also: opening a parameter whose signature contains an anonymous impl is
  `NotAModule` (the impl has no name to project).

## (1) Implemented (2026-09-15, branch `named-signatures`)

Root cause: a module was read as a signature only when the syntactic `module`/`sig`
form sat in a type position (`Elab_type_expr.type_value_of_expr` had a separate
branch for it); a let-bound signature is a module *value* and failed
`check_type_like`. Now one rule reads any module value whose public members are
all types as the signature of modules with those members
(`Elab_validate.signature_of_module`), wherever it was written; the syntactic
branch is deleted. `Sig = sig { x : I64 }; fn(m : Sig)`, a let-bound
`module { pub x = I64 }`, and an imported `S.Sig` all work.

Behaviour change: the deleted branch ignored `pub`, so `fn(m : module { x = I64 })`
used to mean a public `x`. Now a signature's members are the module's public
members, as for any module; tests migrated to `sig { x : I64 }`.

## Still open

- **(2) Dependent signatures need a design.** `sig { T : Type; v : T }` reads `v`'s
  type as the value of `T`'s *declaration* (`Type`), so `v : Type`. A dependent
  signature must bind `T` abstractly (a telescope: `m.v : m.T`), which a
  `VModule` signature (entries of plain values) cannot express. Decide the
  representation (Σ-like telescope of closures, or translucent members) first.
  ```fun
  h = fn(m : sig { T : Type; v : T }) { m.v };
  h(module { pub T = I64; pub v = 3 })   // CannotUnify(Type vs I64)
  ```
- **(3) Opening a parameter whose signature has an anonymous impl** is
  `NotAModule`: the impl has no name to project. Decide how an anonymous impl in
  a signature is reached (e.g. by trait resolution against the parameter, not by
  projection) before implementing.
