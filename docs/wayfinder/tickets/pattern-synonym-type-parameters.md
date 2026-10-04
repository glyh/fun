---
title: "Should a pattern synonym's generalized types be supplyable?"
parent: ../quill-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-27
resolution: "Closed 2026-09-27: ruled 2026-09-25, implemented and merged (`9b0d2b5`), gate 0 build errors / xUnit 186/186 / `conformance: 798 cases, 0 failed` (792 + its 6 new pairs). All three ruled answers are cased. **One deviation from the ruling's route, recorded below and flagged to the user**: the ruling said to reuse `InsertImplicitArgs`/`CheckUnderImplicit` *in place of* the rigid `CollectSynonymMetas`/`InstantiateSynonym` path; what landed keeps that path and adds supply plus scope-end reporting to it, reusing the `WrittenRows` idiom instead."
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

```quill
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
- `dotnet/src/Quill.Compiler/Elaborator.Patterns.cs` (`CollectSynonymMetas`,
  `InstantiateSynonym`), `dotnet/src/Quill.Kernel/Core.Patterns.cs` (`VPatternSynonym`)
- the implicit machinery the other reading would reuse: `InsertImplicitArgs` /
  `CheckUnderImplicit` in `dotnet/src/Quill.Compiler/Elaborator.Implicits.cs`
  (paths in this section are stale — `dotnet/` was lifted to the repository root on 2026-09-26)

## Closed 2026-09-27

Merged as `9b0d2b5` (fast-forward). Gate on the merge: `dotnet build` 0 errors, xUnit 186/186,
`conformance: 798 cases, 0 failed` — 792 before it, plus the six case pairs below.

**What landed.** `MetaContext.SynonymTypeParams` collects the metas minted for a synonym's
generalized types at each use, documented the way its neighbour `WrittenRows` is; the scope-end
check sits in `Elaborator.Effects.cs`, a few lines below the `_`-row check and in the same idiom:

```
a pattern synonym's type parameter is never solved: supply it, as in M.Two[I64, Bool](x, y)
```

`SynonymBaseHead`/`SynonymSupply` peel the implicit applications off a synonym's head, so
`M.Two[I64, Bool]` resolves like `M.Two`; `InstantiateSynonym` takes the supply, rejects more
supplied types than the synonym has parameters, and **solves** each fresh meta with the type
written for it — the way `f[I64]` supplies an implicit. Two places that would misread the
application as a type former are guarded (`RejectFormerHeads`, `RefinementOf`), `HeadLabel`
reports the base head so a message names `Two` rather than the application, and `Enforest.Match.cs`
reads the supplied type arguments to begin with. Reflecting such a use is refused as an unported
path (`Reflection.cs`, `Path`), because the reflected `Path` ADT has no slot for them yet.

**The three ruled answers, each cased:** supplied (`pattern-synonym-supplies-type-params`,
`-leading-type-params`, `-through-open`), reported unsolved where its scope ends
(`pattern-synonym-unsolved-type-param` — the ticket's own negative case, whose scrutinee type is a
bare meta — and `-after-partial-supply`), and an over-supply rejected
(`pattern-synonym-supplies-too-many-type-params`). One rule everywhere follows structurally: a
`match` arm and a lambda parameter both go through `ElaborateSynonymUse`.

### Deviation from the ruling's route — for the user, not buried

The ruling's implementation sentence said to reuse the implicit machinery *in place of* the rigid
`CollectSynonymMetas`/`InstantiateSynonym` path, and that the rigid reading is **not** kept as a
compatibility mode. **What landed keeps that path** and adds two things to it: the supply of
leading parameters, and the scope-end report. The `WrittenRows` idiom is reused for the report
rather than `CheckUnderImplicit`.

The three *answers* are unaffected — they are what the ruling is about, and each has a case. But
the route is not the one written down, so it is recorded here rather than passed over. The case for
the landed route, from reading it: collecting which types a synonym's right-hand side leaves
generalized is a different job from inserting implicits at a *call* site — the first decides what a
declaration's parameters are, the second supplies arguments to a use — and the synonym keeps
needing the first. No semantic difference was found in the ruled cases.

### What the author did not report, and what the integrator checked instead

**There is no fork report for this ticket.** Its first run was aborted mid-diagnosis (it had
committed nothing, so that run was discarded); the resumed run committed the two green steps and
the merge and then stopped responding, so no "what I did not do" exists for it. What was checked
here instead: the diff of all five touched source files, the six cases' `.qll`/`.expect`, and the
gate above. **Not verified by me:** the semantics of a synonym use nested inside another
synonym's right-hand side. The code carries deliberate bookkeeping for it — the declaration saves
and restores `SynonymTypeParams.Count` so a nested use's parameters are not reported as unsolved
at the inner declaration — but no case exercises that path, and nobody has written down what it
should do.
