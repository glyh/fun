---
title: "Port: identity must survive the pipeline's re-evaluation"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: identity must survive the pipeline's re-evaluation

**Ruled by the user, 2026-09-25**, while settling [a former's captures](port-rec-enum-over-captures-names.md):

> Agreed 3 for now but if the compilation pipeline recalculate something, it's probably a bug.

That is a sharper statement than the one it corrects. The model justifies
[applicativity](nominal-identity-applicative-by-purity.md) by *"the checker re-evaluates `Set(I64,
cmp).T` during conversion, so a type minted per evaluation would not equal itself"* — but that is
a **symptom, not the design**. The design is:

> A type's identity is a pure function of its declaration, its own free variables and its
> module's stamp. Re-evaluating the same declaration with the same values **must** give the same
> type — so a recalculation that changes the answer is the bug, not a reason to design identity
> around it.

The doc's own instance of the property is `a.union(x_from_a, y_from_b) -- must typecheck`.

## The audit

Every place the pipeline re-runs a type is a place this property can break. Known candidates, to
be read and then probed (not inferred — this area has had several recorded causes overturned):

- `Nbe.Generative.cs` — `MatchesNominalHead` **evaluates the head term and applies one fresh meta
  per parameter** on every nominal-head match (`Eval(mc, env, head.Head)`, `mc.Fresh()` per
  arity), then compares captures through `SameInstance`. The `ponytail:` note above it records the
  native-stack nesting as a deliberate choice with "no observable difference" — **that claim is
  what this ticket tests.** A fresh *meta* is fine (it is solved); a fresh **stamp**, or an
  unsolved meta landing inside a compared type, is not.
- `Nbe.Patterns.cs:88`'s nested `Eval` — judged *innocent* in
  [nominal identity](port-nominal-identity.md), but that judgement predates the stamp work.
- `Nbe.Force` — forcing a glued/stuck type re-runs its parts.
- the checker's requests to the evaluator during conversion and type-case refinement, each of
  which re-evaluates a type under the budget.

For each: **can it re-run a module *expression* (minting a stamp), or leave an unsolved meta
inside a type that is then compared?** A "no" with a program behind it is the wanted answer.

## The test shape

The property is "a type equals itself after any number of the pipeline's passes". The shape that
exercises it — a type-case on a sealed nominal, a dependent re-check, a conversion — is the fork's
to find; the model's own instance is `a.union(x_from_a, y_from_b)` from
[applicativity](nominal-identity-applicative-by-purity.md). Whatever you find belongs in the
shared suite as an ordinary passing case (`expect` the value both runners give — if the prototype
fails it, that is a divergence to record instead).

## Order

**Queued behind [a pattern synonym over a sealed-nominal head](port-pattern-synonym-over-sealed-nominal-head.md)**: both are `Nbe.Generative.cs`'s comparison code, and running them together
means resolving a conflict blind. When it starts, the capture narrowing
([a former's captures](port-rec-enum-over-captures-names.md)) should already be in.

## Reading

- `dotnet/src/Fun.Compiler/Nbe.Generative.cs` (all of it), `Nbe.Patterns.cs:88`,
  `Nbe.Force`, `Budget.cs`
- [nominal identity is applicative by purity](nominal-identity-applicative-by-purity.md) — the
  decision this generalises
- [the parametric nominal in a generative module](port-generative-former-nominal.md) and
  [the generative former's identity residue](port-generative-former-identity-residue.md) — what
  the stamp is and where it is captured
