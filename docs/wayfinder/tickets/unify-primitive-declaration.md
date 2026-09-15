---
title: One declaration per primitive
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# One declaration per primitive

## Question

A primitive's identity is replicated by bare string across four places that must
stay in lockstep. What is the one declaration the rest should derive from?
Promoted from the design map's fog list, where it was recorded as needing its
own investigation before choosing a scheme.

## Anatomy (corrected — the fog note over-counted)

There are **three** sites, not four, and one of them cannot be fixed by a table:

1. `lib/backend/interp/nbe_prim.ml` — `prim_table : name -> (Atom.t list -> Atom.t option)`
2. `lib/semantic/typecheck/elab_prelude.ml` — `prims : name -> value` (the type)
3. `elab_prelude.stdlib_source` — referenced by string literal from prelude source

`elab_entry.ml` is **not** a fourth copy: it folds over `prims` and binds each as
`HPrim name`. It is already derived.

## A fourth thing a primitive declaration would have to carry: how it fails

Found while probing for unrelated bugs, and worth recording here because it is
the same shape as `panic` not fitting a `(name, type, reducer)` record.

- **Fixed:** `1 / 0` let OCaml's `Division_by_zero` escape the evaluator
  uncaught. The reducer type is `Atom.t list -> Atom.t option`, where `None`
  means *does not reduce*, so a partial primitive had no way to say *fails*
  other than raising. `/` and `%` now raise `Nbe_error.EvalError "division by
  zero"`, the same channel `panic` already uses. Regressions in
  `test/backend/test_core.ml`.
- **Not fixed, deliberately:** `Int64.min_int / -1` wraps to `min_int` here,
  because that is what OCaml's `Int64.div` does. .NET throws `OverflowException`
  for the same expression. Nothing in the source states which is the language's
  answer, so a port will silently inherit its host's. This is a semantics
  decision, not a repair.

Both say the same thing: a primitive is not `(name, type, reducer)`. It is
`(name, type, reducer, failure behaviour)`, and the fourth component is
currently unwritten — implicit in `panic`, in what OCaml's `Int64` happens to
do, and, until now, in an escaping host exception.

Separately, `atom_ty_of_atom` is duplicated verbatim in `nbe_prim.ml` and
`elab_prelude.ml` — trivially collapsible, unrelated to the rest.

## Why a naive `(name, type, reducer)` record fights back

Prims are not one shape. Three exist today:

| shape | example | reducer |
|---|---|---|
| atom-to-atom | `+`, `eq_i64` | `Atom.t list -> Atom.t option` |
| special-cased by name | `panic` | none — hardcoded in `Nbe.try_prim_reduce`, needs the *frame* list and raises |
| type-only | (any future hole) | none |

A single record with a `reducer` field cannot hold `panic`; making the field a
variant (`AtomReducer | Special | None`) is possible, but at that point the
unified table is carrying less weight than it first appeared to. **Anyone
attempting this refactor should decide the variant up front rather than
discovering `panic` halfway through.**

## The payoff is smaller than it looks — measure it before refactoring

Which desyncs actually hurt?

- **name in `stdlib_source`, missing from `prims`** → already loud. Prelude
  elaboration fails inside `init_ctx`, so every test dies immediately. Not a
  problem worth solving.
- **name in `prims`, missing from `prim_table`** → **silent**. `try_prim_reduce`
  falls through to `| None -> None`, the application stays a stuck neutral, and
  evaluation yields a `VNeutral(HPrim …)` where a number was expected. This is
  the whole bug class.

So the value is concentrated in one direction of one pair.

## Cheaper alternative — do this first

Assert the invariant instead of removing the duplication. At `init_ctx`, check
that every name in `prims` is either in `prim_table` or on an explicit
`no_reducer_by_design` list (`panic` today). ~10 lines, catches the silent case,
touches no library boundary, and leaves the tables where they are.

Only pursue full unification if that check keeps firing.

## If unification is still wanted: the structural obstacle

It is three lines in the wrong library. `pure_effects`, `^->` and `^->>` live in
`elab_common.ml` (`core_tt_typecheck`) but construct nothing above `Core` —
`VPi`, `Pi`, `effect_row_closure`, `empty_effect_row` are all kernel. Move them
down to the kernel and the type table can sit beside the reducer table in
`core_tt_interp`, which `core_tt_typecheck` already depends on. There is no
dependency cycle to break; the combinators are simply in the wrong place.

## Why it blocks the port

Whichever route: the mismatch is invisible to the compiler today and would be
equally invisible after transcription into .NET. Settle the shape (and the
`panic`-style exceptions) before the prim set is written out a second time.

## Resolution

**The cheap alternative this ticket proposed is done; the unification question
stays open.**

- `Elab_prelude` now checks at module-initialisation time that every name in
  `prims` either has an entry in `Nbe_prim.prim_table` or appears on an explicit
  `prims_without_reducer` list (`panic` today, with the reason written down).
  This catches the one silent direction identified above; the other direction was
  already loud.
- Confirmed the check fires rather than decorates: adding a typed primitive with
  no reducer aborts every test binary with the name in the message.
- `atom_ty_of_atom`, duplicated verbatim, is now an alias to the copy in
  `nbe_prim.ml`, which is the one that sits beside the reducer table.
- Division by zero was fixed here too — see the section above.

Full unification is untouched and should stay that way until the assertion
actually fires. The `panic` variant question and the `elab_common` combinator
move both still stand as written.

## Grilled (2026-09-15): overflow is an error; one declaration per primitive

1. **Integer overflow is a run-time error**, like division by zero: `+`, `-`, `*`,
   `/`, `%` on `I64` that overflow (including `min_int / -1`) fail with an
   evaluation error naming the operation. During type checking the same failure
   is an elaboration error (small-followups item 6). The port writes checked
   arithmetic explicitly rather than inheriting the host's behaviour.
2. **One declaration per primitive**: name, type, reducer and failure behaviour in
   one record, e.g. `{ name = "+"; type = I64 -> I64 -> I64; compute = add;
   fails = Overflow }`. The reducer is a variant (atom reducer | special, as
   `panic` | type-only), decided up front. Move `pure_effects`, `^->`, `^->>` to
   the kernel so the type sits beside the reducer; the prelude source refers to
   primitives through the table; the `prims_without_reducer` list and the
   duplicated `atom_ty_of_atom` go.
