---
title: ADTs are declared by let bindings (`enum` expressions); `type` is deleted
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
decided: 2026-09-15
assignee:
blocked_by:
  - nominal-identity-applicative-by-purity.md
---

# ADTs are declared by let bindings (`enum` expressions); `type` is deleted

## Decision (grilled 2026-09-15)

Like records ([records-only-let-bindings](records-only-let-bindings.md)), a nominal
ADT is a value declared by a let binding. The `type` keyword goes.

```fun
Color = enum { Red, Green, Blue }
Option = fn(A : Type) { enum { Some(A), None } }
rec Tree = enum { Leaf, Node(Tree, Tree) }
rec Expr = enum { Lit(I64), Block(Stmt) }
and Stmt = enum { Do(Expr) }
```

(`enum { … }` is the working spelling; settle it when implementing.)

## Consequences, and what each needs

1. **Same arguments give the same type.** `Option(I64)` must equal
   `Option(I64)`, so evaluating `enum { … }` under a function cannot mint a fresh
   type per call. Decided as E11 (nominal identity is applicative by purity),
   **not implemented**: today a nominal declared under a binder does not
   evaluate at all (`mk(I64)(1)` → `unbound nominal type`). Hence `blocked_by`.
2. **Constructors are members.** `Color.Red`, or `open Color; Red` — as struct
   members. `type` today puts `Red` in scope directly. The prelude opens its own
   (`Option`, `List`, …).
3. **Constructors of a type function.** `Option.Some(1)` applies `Option` to a
   fresh `A` and takes `Some` — the rule already decided for pattern heads
   ([pattern-head-accepts-type-formers](pattern-head-accepts-type-formers.md):
   aliases work), now in expressions too.
4. **Mutual recursion is `rec … and …`.** The `type … and …` three-phase knot
   moves onto the let group shared with recursive records, so an ADT and a
   record may sit in one group.
5. **Indexed families are not expressible** (`Vec(n)` with `Nil : Vec(0)`,
   `Cons : Vec(n) -> Vec(n + 1)`): `fn(n) { enum { … } }` cannot give a
   constructor its own index. Nothing uses them today; a later `enum` extension
   would be needed.

## Work

- After E11: `enum` expression, constructor members, `Option.Some` through a
  former, `rec … and …` knot, delete `TypeBinding`/`TypeDef` and the `and` chain.
- Migrate the 15 prelude types, compiler-known `Expr` ADTs and their pattern
  synonyms (see `CLAUDE.md`, "Adding a new Syntax ADT"), and every test.
