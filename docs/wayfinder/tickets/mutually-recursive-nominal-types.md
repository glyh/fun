---
title: Mutually-recursive nominal type declarations
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee:
blocked_by:
---

# Mutually-recursive nominal type declarations

## Question

Let two (or more) nominal type declarations reference **each other** —
`type A = … B …` together with `type B = … A …` — so they elaborate as a group,
in any order. Today only **self**-recursion works.

## Context

- **Verified gap.** The elaborator registers a *self-placeholder* for a nominal in
  the context *before* elaborating its constructors (`elab_infer.ml:106-109`,
  `:159-175`), which is why `List`/`Expr` (self-recursive) work. But a type that
  references a **later-defined** type fails — `do type A = MkA(B); type B = MkB(A); … end`
  errors `UnboundVariable "B"` (and the reversed order errors on `A`). There is no
  `and` / type-group surface syntax (the `and`s in `enforest.ml` are OCaml, not
  `fun`). Self-reference through a structural type works
  (`List(Pattern * Expr)`, `List(struct … end)`); only *nominal↔nominal* mutual
  reference is blocked.
- **Why it matters now.** It blocks the dedicated `Branch` ADT in
  [Reflect Match in the Expr macro ADT](reflect-match-in-expr-macro-adt.md):
  `Expr` has `RawMatch(…, List(Branch))` and `Branch` carries an `Expr` body, so
  they are mutually recursive. (An effect-branch variant would carry an `Expr` body
  too, so no branch *nominal* is expressible without this.) The grilling on that
  ticket chose to build this capability first rather than fall back to a structural
  tuple.
- **Beyond reflection.** This is a general language gap — any user AST-like pair of
  datatypes (expr/stmt, tree/forest, …) hits it. Worth doing for its own sake.

## Design questions (to grill)

- **Surface syntax:** explicit `type A = … and B = …` groups (ML/OCaml style), or
  make all type declarations within a `do`-block / module / binding group
  implicitly mutually recursive (register every type name as a placeholder before
  elaborating any constructor set)?
- **Elaboration:** two-pass over a group — (1) push a placeholder `VNominal` for
  every type in the group, (2) elaborate all constructor payloads against the full
  set, (3) tie the knot (replace placeholders with the finished nominals). How this
  composes with the existing single-nominal placeholder path and with
  `Ctx.define`/`TypeBind`.
- **Interaction with the type-aware macro interleaving / binding groups** — the
  driver already advances declarations one binding at a time; a mutually-recursive
  type group must be advanced as a unit.
- Records too (mutually-recursive record types), and parameterized types
  (`type A(X) = … B(X) …`).

## Resolution

_Unresolved._
