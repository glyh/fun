---
title: Mutually-recursive nominal type declarations
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee: glyh
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

_Unresolved (design decided via grilling 2026-09-06 — see Decided below;
implementation pending)._

## Decided (grilling, 2026-09-06)

- **Surface syntax: explicit `and` groups** (OCaml style). Separate `type`
  statements stay sequential — forward references between separate statements
  keep erroring with the plain `UnboundVariable` error (no special
  "use `and`" hint — grilling decision). One group is one statement,
  so it is one advancement unit for the semantic driver, and hygiene/scope work
  happens in one node rather than across adjacent bindings.
- **Nominal-only chains.** Records are excluded from `and` chains; the deferred
  remainder is tracked in
  [mutually-recursive-record-types.md](mutually-recursive-record-types.md).
  Parameterized and zero-parameter nominals may mix within a chain.
- **One binding node per chain.** A chained statement becomes a single binding
  node carrying the member list through every pipeline stage; visibility is
  chain-level (`pub` applies to all members); duplicate member names within a
  chain are a hard error. The expander introduces fresh scopes for all member
  names at once; the semantic driver advances the chain as one unit.
- **Scoped do-heads rejected.** `do type A = … and B = …; body` heads accept
  exactly one type; chains there raise a targeted error. Full support tracked in
  [type-def-chains-in-scoped-do-heads.md](type-def-chains-in-scoped-do-heads.md).
- **Shared group elaborator.** Extract one helper that elaborates a list of
  nominal declarations (a single declaration is the n=1 case) and route both
  existing call sites through it — the per-binding module path and the inline
  copy in the struct/do-body fold — deleting the duplicated code. The two copies
  have drifted; the existing test suite locks the merged behavior.
- **Three-phase knot.** (1) Register: push an empty placeholder (fresh nominal
  id) for every member, in chain order, into the elaboration context. (2)
  Elaborate: typecheck every member's constructor payloads against the context
  holding all placeholders. (3) Finish: build the completed nominals (same ids),
  then add constructor and type names to the context in chain order. Placeholder
  and finished nominal share one id, so payloads written against placeholders
  behave as if written against the finished types — exactly the mechanism that
  makes today's single self-recursive types work, run for several placeholders.
  Constructor values are built against an env whose member slots hold the
  finished nominals, mirroring today's single-member arrangement.
- **Diagnostics: no hint.** Separate-statement forward references keep the bare
  `UnboundVariable "B"` error. No scan of later bindings to suggest `and`.
