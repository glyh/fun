---
title: Nbe's module re-evaluation drops Vars and Bounds
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by: []
---

# Nbe's module re-evaluation drops `Vars` and `Bounds`

Reported 2026-09-27 by the fork that implemented
[a generic impl's head variable carries no bound](generic-impl-head-var-has-no-bound.md),
as a pre-existing hazard it met while working. **Not independently reproduced by the
integrator** — recorded because an impl's bounds now travel as data
(`TraitEvidence.Bounds`, `ModuleEntry.Impl.Bounds`), so anything that rebuilds a module
value has to carry them.

## The claim

A module value re-evaluated by `Nbe` loses its entries' `Vars` and `Bounds`: the
rebuilt `ModuleEntry.Impl` keeps the name, kind and dictionary type but not the
variables the impl's head bound, nor the dictionaries it now takes. If the rebuilt entry
is then exported or selected, an impl that was generic becomes one that is not — silently,
since the head's variables are not part of the name or the dictionary type.

The fix in that ticket worked around nothing here: the paths it exercised (top-level,
module and unit impls, through `open`, `export` and `import`) keep their `Bounds`, which
is how the gate passes. This ticket is for the paths nobody measured.

## Why it is plausible rather than speculative

A `ModuleEntry.Impl` is a record: a re-evaluation that reconstructs it from the *value*
layer has only what the value carries, and the value is a dictionary — evidence erasure
is the design ([traits](../topics/traits.md): trait evidence is not a user-facing value).
So the safe shape is that `Vars`/`Bounds` live in a place a re-evaluation cannot lose,
and an implementation that keeps them only in the entry is one reconstruction away from
dropping them.

## What to measure before designing anything

1. Construct a module value that is re-evaluated and then exported — the shortest is a
   module built by a function and returned, then `export`ed or selected from.
2. Read the resulting `ModuleEntry.Impl.Vars`/`.Bounds` and compare with the source impl's.
3. Only if they differ: decide whether the fix is that the entry is carried rather than
   rebuilt, or that the impl's `Vars`/`Bounds` are derivable from something the value
   already has (its head, its dictionary type) — the second is cheaper but only works if
   the head survives in the dictionary type.

## Reading

- `src/Fun.Compiler/Nbe.cs`, `Nbe.Generative.cs`, `Nbe.RecTypes.cs` — where a module
  value is rebuilt
- `src/Fun.Compiler/Elaborator.Traits.cs` — `TraitEvidence.Bounds`, `ImplBound`,
  `ModuleEntry.Impl`
- `src/Fun.Kernel/Core.Traits.cs` — `ModuleEntry` and what a re-evaluation can reconstruct
- [port: identity must survive re-evaluation](port-identity-survives-reevaluation.md) —
  closed, and the closest earlier encounter with this family of problems
