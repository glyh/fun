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

_Unresolved._
