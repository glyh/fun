---
title: Impl resolution takes the innermost impl, not the most precise matching one
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-27
resolution: Closed 2026-09-27. A fork implemented all four scope items (core fix ca897c8, the rest recovered by the integrator as 6262eb6 after the fork was killed by an extension reload before it could report or commit), and the integrator verified and merged it. The baseline that failed now answers: impl Size(Option(A)) with Size.size(Some(5)) was 'unbound variable: A', is 2. Conformance 790 cases 0 failed, xUnit 186/186 (one test added). Both open items the reconnaissance left are answered as cases - a generic impl through open (2), through export (3), through an import (5). This unblocks pattern-headed-impls.
assignee:
blocked_by:
---

# Impl resolution takes the innermost impl, not the most precise matching one

Recorded as a follow-up by the C# port's traits fork (2026-09-16).

## Decided (user, 2026-09-17): the most precise matching impl

```
{ trait Size(A) = sig { size : A -> I64 };
  impl Size(I64) = module { size = fn(n) { 1 } };
  impl Size(Char) = module { size = fn(c) { 2 } };
  Size.size(5) }
```

gives 1. Resolution follows the rule written in
[traits.md, "Resolution"](../topics/traits.md): candidates are the in-scope impls
matching the trait *at the use's argument types* (for `Trait.op` and bounds alike);
the most precise one (whose arguments are an instance of every other candidate's)
is chosen; no unique most precise one is an ambiguity error; unknown argument types
make the choice wait, and still-unknown at the end is an error; lexical nearness
never breaks a tie. This replaces the earlier "multiple matching impls: ambiguity
error". Both the prototype and the port take the innermost impl here and fail with
`CannotUnify(Char vs I64)`. **To be fixed in the C# port only** (the `ponytail:`
note in `Elaborator.Traits.cs`); the prototype keeps the defect.

## Conformance

`values/trait-op-resolves-by-argument` (1), listed in
`test/conformance/prototype-divergences.txt`. The implementing fork adds cases for
precision (a generic and a specific impl), incomparable candidates (ambiguity) and
waiting on unknown argument types.

## Fixed in the port (2026-09-17)

Merged from `port/impl-precision` (`b3c168f`, `9614dc6`). `Trait.op` and bounds share
one resolution path by argument type (rules 1, 3, 5); a choice with unknown argument
types waits until the end of its unit, then fails with "cannot choose an
implementation" (rule 4). Shared cases: `values/trait-op-resolves-by-argument` (1),
`values/trait-impl-per-argument` (2), `elaborate/trait-op-nearness-no-tiebreak`
(error) — all failing in the prototype and listed; `values/trait-choice-waits-for-argument`
(7) agrees. C# 340/678; xUnit 135.

**Rule 2 has nothing to order yet.** An impl today has no type variables of its own
(`impl Size(I64)`; `impl Eq(T)` names a `T` already in scope), so any two matching
impls are instances of each other and more than one match is an ambiguity. The
precision order applies once impls can be generic — `impl Size(Option(A))` with its
own `A` — which is a language feature not yet designed (syntax, and how an impl's own
variables are bound). `ResolveEvidence` marks where candidates get ordered.

## Grilled (2026-09-18): generic impls — implicit binding

Rule 2's precision order has nothing to order until impls can be generic. Decided:

**A free name in an impl's head binds, with no declaration.**

```
trait Size(A) = sig { size : A -> I64 };
impl Size(I64) = module { size = fn(n) { 1 } };
impl Size(Option(A)) = module { size = fn(o) { 2 } };   -- A is this impl's own
Size.size(Some(5))                                       -- 2; Option(A) is the precise match
```

**Why implicit, not a binder list.** An impl's head is *matched* against the use's
argument types, and a free name in a pattern already binds without declaration
(`match (c) { Some(a) => a }`; `pub pattern Var(name) = Expr.RawVar(_, name)` in
`std/stage1.fun`). Declaring impl variables would make the head the one matched
position in the language that needs them declared.

**Rejected: `impl[A] Size(Option(A))`.** `[…]` means *omittable at application* —
`fn[A : Type](lhs, rhs)`, `[A : Eq] -> A -> A -> Bool` (`std/stage2.fun:23`). An impl
is never applied; its variables are solved by matching. Borrowing the bracket would
give it a second meaning. `impl(A) …` was also rejected: `(…)` after a keyword or a
defined name means that thing's parameters (`trait Size(A)`, `type Option(A)` →
`rec Option = fn(A : Type) { … }`), and an impl's parens belong to the trait it
applies.

**Cost accepted.** `impl Size(Optoin(A))` is a typo that silently becomes a generic
impl over two fresh variables; it never matches, and the error surfaces at the use
("cannot choose an implementation"), not at the definition. Diagnostics are deferred.

**Ambiguity fails, unchanged (rule 3).** Incomparable heads are an error:

```
trait Conv(A, B) = sig { conv : A -> B };
impl Conv(I64, B) = module { … };     -- any B
impl Conv(A, Bool) = module { … };    -- any A
Conv.conv(5) : Bool                    -- both match, neither is an instance of the
                                       -- other: "cannot choose an implementation"
```

A blanket head is *not* ambiguity: `Option(A)` is an instance of `_`, so it is
strictly more precise and wins.

**Deferred to [pattern-headed impls](pattern-headed-impls.md)** (decided 2026-09-18,
split): pattern synonyms as heads, or-patterns, blanket `_`, and changing
`Syntax.Decl.DeclImpl`'s arguments from `List(Expr)` to `List(Pattern)`. Nothing needs
them yet; this ticket only needs a head that binds its free names.

## Reconnaissance (2026-09-25) — from a fork that died before writing code

A fork ran the investigation below and then hit the provider's 5-hour cap (`429`) on the step
before implementing, so it committed nothing. **These are leads from its transcript, not verified
conclusions** — nothing was built or run. Taken together they say the implementation is smaller
than the ticket implies, and where its risk sits:

- **One production site.** `TraitEvidence` is created in exactly one place,
  `Elaborator.Traits.cs`, plus the bound-dictionary path at `Elaborator.Implicits.cs:83` — and that
  one is *non-generic*. So the impl's own-variables list (`Vars`) has a single site to thread
  rather than a general representation change.
- **The matching direction may already be free.** In `Unify`, `(VVar a, VVar b) when a.Level =
  b.Level` unifies *rigidly* while metas solve. So binding an impl head's free names as **fresh
  metas** gives one-way matching — impl-side variables get solved against the use's argument types,
  while the use's own variables and metas stay rigid — without inventing a matching mode.
- **Where a head's free name has to be bound.** `impl Size(Option(A))`'s head elaborates through
  `Infer(ctx, argSyntax)` to `Ap(Var Option, Var A)` and then applies the type function, producing
  the nominal `Option(A)`. So the free names must be in scope via `Bind` **before** the head's
  argument syntaxes are inferred; the collection point is there, not in the resolver.
- **Open item it did not reach:** how an imported `pub impl`'s evidence reaches the importer
  (`ModuleEntry.Impl` uses) — it reasoned only that head *values* persist with the evidence and that
  the variables list is the sole new thing, then died before checking.
- **Hazard it flagged and left open:** `values/trait-impl-through-open` shows the `open` path
  matters, and it suspected a generic impl reached through `open` could break. It recorded the
  suspicion as unverified, which is the honest state: **probe it** rather than assume either way.
- Its own next step was "baseline: build and test first" — i.e. it had not yet measured anything.
  Whoever takes this should start with the precision program from the ruling above and the probes
  for the two open items.

## Closed 2026-09-27

**Provenance, because it matters here:** the implementing fork (Oracle, on deepseek-flash after the
GLM provider 429'd) was **killed by an extension reload** partway through. It had committed the core
fix (`ca897c8 traits: an impl head's free names bind as its own type variables`) but not the rest, and
the reload also dropped it from the agent registry, so `resume` was impossible. Its worktree
survived; the integrator committed the remaining tree onto a branch and merged it (`6262eb6`). So the
work below is the fork's, verified by the integrator — not a fork's report read at face value,
because no report was ever written.

**The baseline this ticket was opened on, measured before and after:**

| program | before | after |
| --- | --- | --- |
| `impl Size(I64)` / `impl Size(Char)`, `Size.size(5)` — the 2026-09-17 ruling's program | `1` | `1` (control, untouched) |
| `impl Size(Option(A))`, `Size.size(Some(5))` | `unbound variable: A` | **`2`** |

**What landed**, all four scope items:

1. `BindHeadNames` (`Elaborator.Traits.cs`): the head's free names are collected by whether the
   context resolves them, each pushed as a definition of a fresh meta around the *head's own*
   inference, and the head's open choices are rewritten to those variables.
2. `Matches` replaced the old `Convertible`: readback equality, else a structural unification in
   which **only the impl's own variables may solve** (`vars.Contains(i)`), with every meta restored
   afterwards so the impl stays generic and the use's unknowns stay rigid. That is the one-way
   matching the reconnaissance predicted came for free, and it is what keeps rule 4 (a choice waits
   on an unknown argument type) working.
3. The most precise candidate wins — `p` such that every other candidate is an instance of it — and
   anything else is `ambiguous implementation of \`Conv\``.
4. The impl's own variables are threaded through `TraitEvidence`, `ModuleEntry.Impl` and the
   `export` path.

**Both open items the reconnaissance left are answered as cases**, which is the strongest form the
answer could take: a generic impl survives `open` (`values/trait-generic-impl-through-open`, `2`),
`export` (`…-through-export`, `3`), and an **imported unit** (`imports/trait-generic-impl-through-import`,
`5`, with its own `unit-lib.fun` publishing the generic impl; `…-in-imported-module` alongside).
The `export` path needed a real change — `Elaborator.Export.cs` had no place to carry `Vars` — so
item 4 was not bookkeeping after all.

**The cost was taken exactly as ruled.** The head's free names bind with no declaration, and the
accepted consequence lives in the code as a comment rather than a check: `impl Size(Optoin(A))`
silently becomes a generic impl over two fresh variables, never matches, and surfaces at the use. No
scan, no warning, no validation.

**Negative controls, all unchanged:** `trait-op-resolves-by-argument`, `trait-impl-per-argument`,
`trait-impl-through-open`, `trait-bounded-call`, `trait-choice-waits-for-argument` (`7`),
`elaborate/trait-op-nearness-no-tiebreak` (`error`), `elaborate/trait-impls-ambiguous` (`error`),
`elaborate/trait-impl-needs-open` (`error`). One test was added, asserting the incomparable-head
message exactly (`TraitTests.IncomparableGenericImplsAreAmbiguous`); xUnit 185 → 186.

**A probe of mine was ill-formed, and its error change is not a regression:** `open Size; size(Some(5))`
gave `unbound variable: A` before because the impl head failed first; it now reaches the real error,
`open of a non-module`, because `Size` is a trait. The `open` answer proper is the case above.

The two deferrals stand: blanket `_`, or-patterns and pattern-synonym heads belong to
[an impl head is a pattern over types](pattern-headed-impls.md), which this ticket unblocks.
