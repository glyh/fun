---
title: "Should a pattern synonym's generalized types be supplyable?"
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Should a pattern synonym's generalized types be supplyable?

> ## Resolution (user, 2026-09-25): they are the synonym's implicit type parameters
>
> The ticket's own recommendation was taken. All three questions answer from the one reading:
>
> 1. **Supplyable — yes.** `M.Two[I64, Bool](x, b)`, the way an implicit is supplied at a call
>    (`f[I64]`), because that is what they are.
> 2. **Reported unsolved** where their scope ends when nothing determines them — the shape an
>    unsolved implicit already has, so the silent corner the ticket describes goes away.
> 3. **One rule everywhere** — a `match` arm and a lambda parameter alike.
>
> Implementation reuses the existing implicit machinery (`InsertImplicitArgs` /
> `CheckUnderImplicit`, `Elaborator.Implicits.cs`) in place of the rigid
> `CollectSynonymMetas`/`InstantiateSynonym` path as it stands. The rigid reading is **not** kept
> as a compatibility mode: nothing in the ruled cases distinguished the two, so there is nothing
> to preserve. The cost the recommendation named — "cannot be supplied, cannot be reported
> unsolved" — is exactly what is being paid.
>
> **Port work, not yet in flight**; it queues behind the two ruled tickets
> ([the unused type parameter](port-generative-former-phantom-parameter.md),
> [the budget call stack](port-budget-attribution.md)) and the E11 chain. Negative case to add
> with it: a use whose scrutinee type is a bare meta, to prove the scope-end report fires.

**Not blocking anything.** Raised while implementing
[a pattern synonym is checked, and generalizes where its type is unknown](port-pattern-synonym-generalizes.md)
(2026-09-24): the port had to pick a reading to land that ruling, picked one, and the
other is recorded here rather than left in a conversation. Nothing in the ruled cases
distinguishes them, so this is a design question, not a defect to fix.

## What exists now

A synonym whose right-hand side cannot determine its scrutinee's type has those types
generalized — `pattern Two(a, b) = (a, b)` becomes a synonym with two type parameters —
and each **use** instantiates them afresh. The port takes the **rigid-variable** reading:
the parameters are fresh metas solved by unification against the scrutinee's type, and
nothing else. In particular a use cannot *supply* them:

```fun
{ M = module { pub pattern Two(a, b) = (a, b) }; open M;
  match ((1, True)) { M.Two(x, b) => x } }        -- works: the scrutinee's type solves them
-- nothing like this exists today:
-- match (v) { M.Two[I64, Bool](x, b) => x }
```

The other reading — the one a *generic function* invites, since the ruling says "generic
... just like generic functions" — is that the parameters are the synonym's implicit type
parameters, solved the way `f[I64]` solves an implicit: supplyable explicitly, and
reported as **unsolved at the end of their scope** when nothing determines them.

## Why it is not just academic

The two readings differ where the scrutinee's type does **not** determine a parameter:

- a use whose scrutinee type is itself a meta (inside an unannotated lambda) — the port
  currently instantiates and lets it stay stuck, and has no scope-end unsolved-meta
  report, so a genuinely unsolvable use can go unreported;
- a synonym used where only part of the scrutinee's type is known.

## Questions to settle

1. May a use **supply** the generalized parameters explicitly, as a generic function's
   can? If yes, what is the spelling — `M.Two[I64, Bool](…)`, the way an implicit is
   supplied at a call?
2. If not, is "stuck until something solves it" the intended end state, or should an
   unsolved generalized parameter be **reported** where its scope ends (the shape an
   unsolved implicit already has)?
3. Does the answer depend on the pattern's position (a `match` arm vs a lambda
   parameter), or is it one rule everywhere?

Recommendation when this is taken up: make them the synonym's implicit type parameters.
**Taken (user, 2026-09-25) — see the Resolution above.**
It is the reading the ruling's own words invite, it answers all three questions with an
existing mechanism, and it costs the rigid reading only the "cannot be supplied, cannot
be reported unsolved" corner.

## Reading

- [a pattern synonym is checked, and generalizes where its type is unknown](port-pattern-synonym-generalizes.md) — the ruling and its Resolution, which
  states which reading the port took
- `dotnet/src/Fun.Compiler/Elaborator.Patterns.cs` (`CollectSynonymMetas`,
  `InstantiateSynonym`), `dotnet/src/Fun.Kernel/Core.Patterns.cs` (`VPatternSynonym`)
- the implicit machinery the other reading would reuse: `InsertImplicitArgs` /
  `CheckUnderImplicit` in `dotnet/src/Fun.Compiler/Elaborator.Implicits.cs`
