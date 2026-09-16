---
title: Effects on the arrow (Unison style); `~>` for effect polymorphism
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-16
resolution: Implemented (branch effect-arrows). Rows sit on arrows (`A ->{E} B`, open `{E | e}`, `->{_}` inferred and an error when nothing solves it); `~>` mints row variables in parameter positions and collects them in result positions (a definition's result also infers what its body performs); `can` is deleted; definitions keep `: T` when pure and take `->{E} T` / `~> T` when effectful. Rows carry a set of row variables, so a result unites its callbacks' rows; a curried `~>` collects on its final arrow. Trait bounds are a set `[A : {Eq, Show}]`.
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

**Definitions: `: T` when pure, the arrow form only when effectful** (revised same day):
```fun
double = fn(n : I64) : I64 { n + n }            // pure
bump   = fn(n : I64) ->{Log} I64 { … }          // effectful
app    = fn(g : Unit ~> I64) ~> I64 { g(()) }   // polymorphic
pub method tick() ->{Exc} I64 { … }
```
`fn(…) -> T { … }` (a pure arrow on a definition) is an error suggesting `: T`, so
each case has one spelling. Methods and `Mutate(r)` rows follow (`->{Mutate(r)}`).

## Supersedes

- `can {…}`, `can _`, `can any`, the prelude `~>` = `-> … can _`.
- The "`_` in a parameter becomes a hidden row parameter" proposal (not adopted;
  `~>` covers it).
- `can any` / `->{any}` — **dropped** (grilled 2026-09-16): no "may perform anything" row; code whose effects vary uses `~>` or named row variables. Truly dynamic code is not supported.

## Work

Parser (arrow + brace group, `~>` desugaring with polarity), elaborator (row
variables, union, body inference for result `~>`), reflection, prelude, every
effectful signature and return annotation in tests and docs.

**Open rows (2026-09-16):** a row with a tail is written `A ->{Log, Exc | e} B`
(`|` then the row variable), as `{E | r}` rows were. Rows and bound sets are
dedicated syntax in their positions for now; generalising to one set literal is
[general-set-literals](general-set-literals.md).

## Implemented (2026-09-16, branch `effect-arrows`)

- **Rows on arrows.** `->` followed by an adjacent `{ … }` is the arrow's row
  (`Enforest.parse_postfix_infix`). `->{e}` alone is the tail `e`; beside effects
  the variable is written after a bar, `->{Log | e}` (`{Log, e}` is
  `RowVariableAmongEffects`).
- **`~>`** is a base role (`Syntax.PolyArrow`), read like `->`, whose row is marked
  `polymorphic`. `Elab_poly_arrows` rewrites a signature by polarity before it
  elaborates: a type's root is a result position (minted variables become leading
  implicit `EffectRow` binders), a lambda's parameter types are parameter
  positions (minted variables become leading implicit parameters). Check mode
  binds an implicit `EffectRow` parameter a term does not bind itself. A `~>`
  anywhere else is `PolyArrowOutsideSignature`.
- **Definitions.** `fn(x : A) ->{E} T { … }` / `~> T` annotate the function with
  its arrow type (every parameter typed); `fn(…) -> T { … }` is an error naming
  `: T`. Methods take `->{E} T`.
- **`->{_}`** is a meta abstracted over the row variables in scope only
  (`Ctx.fresh_row_meta`), recorded in `MetaContext.written_rows`; checked against
  a body it is exactly what the body performs, and one still unsolved at a
  program's entry is `UnsolvedEffectRow`.
- **Fixed on the way:** row unification only accepted a left row whose effects the
  right row named (`{Exc}` against `{| ?m}` failed); leftovers now go to the other
  side's tail from both sides. A method's row known before its body is unified
  with the row the body solved.
- **Multi-tail rows (2026-09-16):** a row is known effects plus a set of row
  variables, so `f : (A ~> B) -> (C ~> D) ~> E` works
  ([multi-tail-effect-rows](multi-tail-effect-rows.md)). Rank 1: variables minted
  under a higher-order parameter are bound at the root (`ponytail:`).
- **Curried `~>` (2026-09-16):** polarity is gone. Every function type is read on
  its own: each parameter's type is a signature in its own right, and the chain's
  final arrow carries what its parameters minted, so `twice : (A ~> A) ~> A ~> A`
  is `[e] -> (A ->{e} A) -> (A ->{e} A)` and `twice(f)` is pure.
- **Trait bounds** are a set, `[A : {Eq, Show}]` (`Syntax.TraitBoundSet`); a
  trait named twice is `DuplicateTraitBound`; the `+` spelling recognition is
  deleted. Bounds are still read only on an implicit arrow (`[A : …] -> …`); a
  bound on a lambda's implicit parameter (`fn[T : Eq]`) was not supported before
  and is not now.

## Grilled (2026-09-16), curried `~>`: only the final arrow collects

Arrows are right-associative, so a multi-argument `~>` type is curried. A result
arrow that only *returns another function* collects nothing; the parameters'
variables land on the **final** arrow (where the callback is actually called),
together with whatever the body performs there.

```fun
twice : (A ~> A) ~> A ~> A
//    = [e] -> (A ->{e} A) -> (A ->{e} A)      // twice(f) is pure
```
This also applies inside nested higher-order parameters (the rule is recursive:
each function type mints from its own parameters and collects on its own final
arrow). Needs [multi-tail rows](multi-tail-effect-rows.md) when more than one
parameter mints.

## Grilled (2026-09-16), a standalone `~>` alias mints

A `~>` written outside a signature's parameter/result structure (a type alias,
a field or member type) **mints its own row variable**:

```fun
Callback = Unit ~> I64;              // = [e] -> Unit ->{e} I64
app = fn(g : Callback) : I64 { g(()) }    // ERROR: the result is declared pure,
                                          // but calling g performs e
app = fn(g : Callback) ~> I64 { g(()) }   // ok: the result collects g's e
```
So a use of the alias behaves exactly like writing `Unit ~> I64` in that
parameter position, and a caller's pure result annotation is rejected rather than
silently widened.

## Not implemented: a standalone `~>` alias at its use site (2026-09-16)

The minting half landed: `Callback = Unit ~> I64` now elaborates to
`[e : EffectRow] -> Unit ->{e} I64` (it no longer silently means pure). But a
*use* of that alias reads it as a **rank-2** type - the value must work for
every row - which is the opposite of what the ticket asks:

```fun
Callback = Unit ~> I64;
app = fn(g : Callback) : I64 { g(()) }     // accepted today; ticket wants an error
app = fn(g : Callback) ~> I64 { g(()) };
app(lg)                                     // rejected today (lg is not polymorphic)
```

For the ticket's behaviour the alias's binder must be **lifted to the enclosing
definition** (rank 1): `fn(g : Callback)` would bind `e` as an implicit parameter
of `app`, so `app`'s pure result contradicts `g`'s row and its `~>` result
collects it. That is an elaborator change (implicit row binders in a parameter's
annotation are bound by the enclosing lambda instead of instantiated), and it
decides a language question the ticket does not:

**Does an implicit row binder written inside a parameter's type mean rank 1 (the
caller chooses the row) or rank 2 (the callee needs a value polymorphic in it)?**
Today a hand-written `fn(g : [e : EffectRow] -> Unit ->{e} I64)` is rank 2;
lifting changes that spelling's meaning too, or needs the two to be told apart.
