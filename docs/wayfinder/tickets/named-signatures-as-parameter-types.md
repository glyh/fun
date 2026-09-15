---
title: A let-bound signature cannot be a parameter type; dependent signatures fail
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-15
resolution: Implemented (branch named-signatures, then dependent-signatures). A signature is its own value; it is a telescope (Core.Sig under a binder for the described module, VSig closure, Nbe.module_type_of instantiates it with the module), so s.empty : s.T; impls a signature requires are named (name : impl Trait(T)), provided under that name, reachable as s.name and through open.
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

## Grilled (2026-09-15): dependent signatures supported

A signature is a telescope: a later member's type may mention an earlier member.
For a parameter `s : Stack`, member types are read through the parameter
(`s.empty : s.T`); what `T` is becomes known only when a module is passed.

```fun
Stack = sig { T : Type; empty : T; size : T -> I64 };
IntStack = module { pub T = List(I64); pub empty = Nil; pub size = length };
count = fn(s : Stack) { s.size(s.empty) }   // s.empty : s.T
count(IntStack)
```

## Grilled (2026-09-15): impls in signatures are named

An impl required by a signature must be named; an anonymous `impl` in a `sig` is
an error naming the required form. The name is a member: `s.ord_T` reaches it,
and `open s` also brings it into trait resolution.

```fun
Ordered = sig { T : Type; ord_T : impl Ord(T) };
max_of = fn(s : Ordered, a : s.T, b : s.T) { open s; if (a > b) { a } else { b } };
```

## (2) and (3) implemented (2026-09-15, branch `dependent-signatures`)

- **Telescope.** `infer_signature` binds the described module (`sig#self`) and
  defines each member as its projection, so a later member's type quotes as
  `self.T`. The term is `Core.Sig (Module { signature = true })`; it evaluates to
  `VSig` (a closure). `Nbe.module_type_of ty module` instantiates it: at field
  access and `open` with the receiver's value (a parameter stays abstract), and
  when an argument is checked against a signature, with the argument's value.
  Two signatures unify under one fresh module.
- **Named impls.** A sig item `name : impl Trait(Arg)` parses to an
  `ImplBinding` with no fields; its member promises the dictionary type
  (`impl_dict_type`, shared with impl declarations). Matching a module to a
  signature requires a public impl of that name and type. An `open` of a
  parameter pushes the impl's projection. `impl Trait(Arg)` without a name in a
  `sig` is a parse error naming the form.
