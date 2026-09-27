---
title: Design the user-facing library surface of std
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee:
blocked_by:
  - restructure-std-into-bootstrap-and-library.md
---

# Design the user-facing library surface of std

## Question

After [the restructure](restructure-std-into-bootstrap-and-library.md) gives `std/`
a bootstrap layer and a library layer, the library holds a first cut of the List
and Option API. **What should the library's public surface actually be** — names,
signatures, argument order, what is in versus out, and how it is documented?

This is a grilling ticket: the answers are decisions, so it is worked by interview,
one branch at a time.

## Context

- **Blocked by** [Restructure std into a bootstrap layer and a library layer](restructure-std-into-bootstrap-and-library.md),
  which decides the seam and lands the first cut. Design against the real
  `std/bootstrap.fun` plus library units, not against a plan.
- **The seam is already fixed**: the bootstrap layer is exactly what C#
  names by string (`Syntax` + `Bool`/`Option`/`List`). Their *names* and
  constructors cannot move, so the design must work around them rather than
  through them.
- **Two neighbouring tickets own parts of this, deliberately not repeated here:**
  [Design trait library deriving and protocols](design-trait-library-deriving-and-protocols.md)
  owns deriving and protocol-style ops; [Specify stage 11 macro-powered language features](specify-stage-11-macro-powered-language-features.md)
  owns surface features that would arrive as macros.
- **The map's standing caution** is the fog item *"Library-level features vs
  compiler machinery"*: prefer library-level macros and type-case over new
  compiler machinery, and do not let library work drive the compiler agenda.
- **Docs on the shape of the language** the library must use:
  [`CONTEXT.md`](../../CONTEXT.md) for vocabulary, and
  [`docs/wayfinder/topics/trait-module-stdlib.md`](../topics/trait-module-stdlib.md).

## What the library does not have today (measured 2026-09-26)

`std/lib.fun` after the restructure will hold `if`, `i64_to_bool`, the
comparison/arithmetic operators and their fixity, `Eq` + five impls, and nothing
else. Missing:

- **List beyond the first cut**: no `length`, `head`/`tail`, `concat`,
  `filter`, `zip`, `any`/`all`, `nth`, `take`/`drop`, `range`.
- **Option**: only `Some`/`None`; no `map`, `bind`, `unwrap`/`get_or`, `is_some`.
- **String**: nothing but the primitives `eq_string`/`neq_string`
  (`Primitives.cs:59-60`) — no `length`, `concat`, `split`, `chars`.
- **Bool**: `not` only; no `and`/`or` as values (the operators are syntax forms).
- **I64**: operators only — no `min`/`max`, `abs`, `to_string`, and no
  float/`U64`/`U8` types at all (check `Primitives.cs` for the real floor).
- **Traits**: `Eq` only. No `Show`, `Ord`, `Semigroup`, `Monoid`.
- **No docs convention** on any `pub` binding, and no test tree of its own.

## Open branches (to be grilled, one at a time)

1. **List/Option API naming and shape.** There is no precedent to copy, so this
   first cut sets one: positional versus half-open indices, `fold`'s argument
   order and accumulator position, `map`'s function-first convention, whether
   functions are curried or take all arguments, and whether the names come from
   OCaml/ML (`rev`, `fold_left`) or from the surface the language already uses.
2. **`head`/`nth`/`tail` on the empty list**: `Option`, `panic`, or an effect.
   This is a genuine design fork, not a style question, and it decides the shape
   of everything downstream.
3. **Traits the library defines versus the compiler keeps.** `Eq` currently lives
   in `lib`. Should `Show`/`Ord`/`Semigroup` follow, and does a library trait need
   a library-level `impl` story (including how impls are named and opened)?
4. **Documentation**: does the surface get a doc convention, and is there a
   surface-syntax slot for it? (Check whether a doc-comment form exists before
   designing one.)
5. **Importability**: may a program write `import "std/list"` directly, or only
   reach the library through `std`? The restructure leaves `Prelude.Of` able to
   answer this either way, so it is a policy decision.
6. **What is deliberately out**, so the ticket ends: UFCS, FFI and protocol/
   deriving machinery are named as "not now" in the map's fog; confirm the rest.

## Answer

Unresolved.
