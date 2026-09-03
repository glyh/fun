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

## Evidence

For a single prim such as `eq_i64`:

- `lib/backend/interp/nbe_prim.ml` — `prim_table`, the runtime reducer
- `lib/semantic/typecheck/elab_prelude.ml` — `prims`, the type
- `lib/semantic/typecheck/elab_entry.ml` — the base-context binding
  (`Var name → HPrim name`)
- `elab_prelude.stdlib_source` — referenced by string literal from the prelude
  source (`eq_i64(x, y)`, `panic[…]`, …)

No single source of truth. Adding or renaming a prim means editing every copy by
hand; nothing catches a missed one at compile time. The "holes" in the stdlib —
typed slots the prelude expects the compiler to fill — are wired the same way.

## Why it blocks the port

This is the highest leverage-to-effort item before the rewrite. Ported as-is,
the four copies become four copies in C#/F#, transcribed by hand, in a language
where the mismatch is equally invisible. Collapsed first, the port moves one
table.

## Sketch of the work

Investigate the options before choosing — a single registry keyed by name, a
typed prim GADT/DU, generated bindings — and pick for maintainability, not
cleverness. Requirements:

1. One place declares `(name, type, reducer)`.
2. Runtime, elaborator, base context, and prelude all derive from it.
3. A prim referenced by the prelude source but absent from the registry is an
   error at build time, not a runtime `UnboundVariable`.

## Resolution

_Unresolved._
