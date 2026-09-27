---
title: "Port: identity must survive the pipeline's re-evaluation"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-27
resolution: Audited 2026-09-27, by a fork (Oracle, 0ddee37, merged) and re-measured by the integrator. Every site the audit list named came back innocent, each with a program behind it; the ponytail note's "no observable difference" claim held. One residue does not hold - a capture that is a lambda term, evaluated twice, is equal to conversion and different to a type-case on the same pair of types - and it is filed as its own ticket, closure-capture-identity-two-answers, because it needs a ruling rather than a fix. Landed values/core-310 (one declaration, three evaluations, one type). Conformance 776 cases 0 failed, xUnit 185/185.
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

## Closed 2026-09-27 — audited, and the property holds at every site but one

A fork (Oracle, `0ddee37`, merged) probed every site the audit list named, each through `--file`,
and the integrator re-ran the programs the verdicts turn on:

| site | verdict | the probe behind it |
| --- | --- | --- |
| `MatchesNominalHead`'s nested `Eval` (`Nbe.Generative.cs:23`, eval at `:36`) | **innocent** | an *alias* head (`MkT(less)`, a function reducing to a nominal) does re-run the maker on every match — and both scrutinees still match, `11`. The `ponytail:` note's "no observable difference" claim held, **which is what this ticket existed to test.** |
| the one fresh meta per parameter (`:28`) | **innocent** | a parameter binder receives the solved value; a floating meta would stick `g(x)`. Phantom parameters are refused at declaration, which closes the unsolved-meta route. |
| `Nbe.StuckMatch.cs:49` `OpenArm` — the ticket's moved `Nbe.Patterns.cs:88` | **innocent** | a stuck type-case unifies as a neutral (`Unify.Neutrals.cs:32`) and both sides re-evaluate an inline maker to the same type |
| `Nbe.Force` | **innocent by reading** | `ForceLoop` resolves metas and glued values only; the rec-unfold probe agrees (`5`) |
| the checker's conversion and type-case requests | **innocent** | a conversion, a dependent re-check and a stuck match all agree |

One case landed, `values/core-310`: one declaration, three evaluations, one type — the model's own
`Set(I64, cmp).T` shape, answering `23`.

**The residue is one site, and it is not a port regression.** A capture that is a λ *term*,
evaluated twice, is "equal" to conversion and "different" to a type-case on the same pair of types
— because `SameInstance` has no λ arm while `Unify` eta-applies one. The prototype's comparison
ended the same way, so this is inherited parity, and the doc comment records it as deliberate. It
is [its own ticket](closure-capture-identity-two-answers.md): it needs a ruling, not a fix, and the
audit was right to stop rather than pick a reading.

The second runner is gone — the prototype was deleted 2026-09-25 — so "if the prototype fails it,
that is a divergence to record instead" has no subject left. Its behaviour was read from `git`,
not run, and that reading is recorded on the new ticket where it matters.

## Reading

- `dotnet/src/Fun.Compiler/Nbe.Generative.cs` (all of it), `Nbe.Patterns.cs:88`,
  `Nbe.Force`, `Budget.cs`
- [nominal identity is applicative by purity](nominal-identity-applicative-by-purity.md) — the
  decision this generalises
- [the parametric nominal in a generative module](port-generative-former-nominal.md) and
  [the generative former's identity residue](port-generative-former-identity-residue.md) — what
  the stamp is and where it is captured
