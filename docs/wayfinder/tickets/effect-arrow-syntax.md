---
title: Effects on the arrow (Unison style); `~>` for effect polymorphism
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
decided: 2026-09-16
assignee:
blocked_by:
---

# Effects on the arrow (Unison style); `~>` for effect polymorphism

## Why

`A -> B can E` puts the row at the end, so nested function types are hard to read
(`(A -> B can Eff1) -> C can Eff2`: which arrow owns which row?). And `_` in
`can _` meant two things — "infer this one unknown" and "polymorphic in the
callback's effects" — with `~>` (`-> … can _`) inheriting both.

## Decision (grilled 2026-09-16)

**Rows sit on the arrow they belong to** (Unison):

| Arrow | Meaning |
|---|---|
| `A -> B` | pure (`->{}`) |
| `A ->{Log, Exc} B` | exactly these effects |
| `A ->{_} B` | infer; must be solved (unsolved is an error) |
| `A ->{any} B` | may perform anything (replaces `can any`) |
| `A ->{e} B` with `fn[e]…` | polymorphic, named row variable |
| `A ~> B` | polymorphic sugar (below) |

`can` is deleted. `->{…}` is the `->` operator followed by a brace group — no
lexer rule (uniform lexing).

**`~>` — parameters mint, results collect.** Within one signature:
- a `~>` inside a parameter type gets its own fresh row variable;
- a `~>` in result position carries the union of the variables its parameters
  minted, **plus** the effects the body itself performs (inferred); with no body
  (trait / `sig` member) it is just the union;
- applied recursively inside higher-order parameters.

```fun
f : (A ~> B) -> (C ~> D) ~> E
//  = [e1, e2] -> (A ->{e1} B) -> (C ->{e2} D) ->{e1, e2} E
log_map = fn(f : A ~> B, xs : List(A)) ~> List(B) { … perform Log.write(…) … }
//  = [e] -> (A ->{e} B) -> List(A) ->{e, Log} List(B)
```
Two independent callbacks whose effects must stay apart, or a result with extra
effects in a bodiless signature, are written with named variables.

**Definitions look like their type; `:` for return types is dropped:**
```fun
bump = fn(n : I64) ->{Log} I64 { … }
pure = fn(n : I64) -> I64 { … }
app  = fn(g : Unit ~> I64) ~> I64 { g(()) }
pub method tick() ->{Exc} I64 { … }
```
The return form `fn(…) : T { … }` (small-followups item 5) is replaced by
`fn(…) -> T { … }`; methods and `Mutate(r)` rows follow (`->{Mutate(r)}`).

## Supersedes

- `can {…}`, `can _`, `can any`, the prelude `~>` = `-> … can _`.
- The "`_` in a parameter becomes a hidden row parameter" proposal (not adopted;
  `~>` covers it).
- `+` on trait bounds (small-followups item 4) and `can any` at a call — `->{any}`
  meaning at a call site still to be specified during implementation.

## Work

Parser (arrow + brace group, `~>` desugaring with polarity), elaborator (row
variables, union, body inference for result `~>`), reflection, prelude, every
effectful signature and return annotation in tests and docs.
