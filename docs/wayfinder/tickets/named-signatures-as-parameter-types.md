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

**Decided (user, option B):** `sig { … }` evaluates to its own kind of value, a
signature value, distinct from a module. A module whose members are all types is
*not* a signature: `Types = module { pub Id = I64 }; fn(m : Types)` is an error
(`NotASignature "Types"`). Only a signature value — inline, let-bound or imported
— is a parameter type. No value has two meanings.

- **Representation.** `Syntax.Sig { bindings }` (reflected `RawSig`), elaborated by
  `Elab_type_expr.infer_signature` to `Core.Module { …; signature = true }`, which
  evaluates to `VModule { partial = true }` and quotes back to itself. A module
  evaluates to `partial = false` and is rejected in a type position.
- **Root cause of the ticket's bug.** A signature was recognised only by the
  syntactic form in a type position (a special branch in `type_value_of_expr`),
  so a let-bound one was just a module value. The branch is deleted; the value
  carries what it is.
- **Behaviour change.** `fn(m : module { x = I64 })` (a module literal as a type)
  is now `NotASignature`; tests migrated to `sig { x : I64 }`.

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
