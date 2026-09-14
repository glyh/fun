---
title: Each core-term traversal counts a form's binders on its own
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Each core-term traversal counts a form's binders on its own

## Defect

How many environment slots each `Core.term` form pushes before its subterms is
an invariant with no single definition. Every de Bruijn traversal re-derives
it, and they disagree:

| Form | `Nbe.closure_slots` (new, 2026-09-14) | `Elab_defs.shift_term` |
|---|---|---|
| `Match` `ValueBranch (pat, body)` | `d + pat_binder_count pat` | `cutoff` unchanged |
| `EffectBranch` | `d + 1 + pat_binder_count arg_pat` | `cutoff` unchanged |
| `NominalDef` body | `d + num_params + 1 + (num_params > 0) + #ctors` | `cutoff` unchanged |
| `NominalDef` ctor payloads | `d + num_params` | `cutoff` unchanged |
| `EffectDef` ops / body | `d + num_params` / `d + 1` | `cutoff` unchanged |
| binding lists | `Core.binding_slots` | rejects lists longer than one |

The evaluator (`Nbe.eval`) is the ground truth; the others restate it. Either
`shift_term` is wrong on these forms (latent: a shift under a match branch
would move pattern-bound variables) or its callers never reach them — which
the closed [core-traversals-ignore-binding-list-depth](core-traversals-ignore-binding-list-depth.md)
measured only for binding lists. Other restatements: `Elab_generalize.closed_under`,
`Elab_refine.close_recursive_payload_term`, `Unify`'s solution-term builder.

This is the env-width defect class ([env-width-contract-is-unnamed](env-width-contract-is-unnamed.md))
one level down: per term form instead of per binding.

## Direction

One definition in `Core` — the subterms of a form with the number of slots
each sits under (as `binding_slots` did for bindings) — that `Nbe.eval`,
`closure_slots`, `shift_term`, `closed_under` and the payload closer all read.
First instrument `shift_term`'s `Match`/`NominalDef`/`EffectDef` cases to learn
whether the disagreement is live.

## Found by

Reviewing the closure-environment change (2026-09-14).
