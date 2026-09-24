---
title: "Port: a pattern synonym is checked, and generalizes where its type is unknown"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
assignee:
blocked_by:
---

# Port: a pattern synonym is checked, and generalizes where its type is unknown

Ruled by the user on 2026-09-21, while
[the unverified rows](port-unverified-rows.md) were being probed — its row 6, the
pattern-synonym cluster. An earlier draft of this ruling said "the right-hand side is
checked at each use"; the user's refinement is the one below, and it is what the port
implements.

## The ruling

> A pattern synonym is **checked**, and where the scrutinee's type cannot be known it is
> a **generic** pattern, just like a generic function: the unknown types become
> parameters, instantiated where the synonym is used.

So, in order:

- The right-hand side is elaborated **at its declaration** and checked there — as it is
  today. A malformed right-hand side is an error even if the synonym is never used.
- Whatever the right-hand side cannot determine is **generalized** rather than refused:
  the unknown types become the synonym's parameters, exactly the step
  `Elaborator.Generalise.cs` already performs for a `let`, and the same shape as a
  generic function's type parameters.
- Each **use** instantiates those generalized parameters against the scrutinee's type.
- The name still resolves at the **definition** site, so hygiene is unchanged; only the
  type comes from the use.

## The evidence (integrator, 2026-09-21, both runners at `b73a755`)

All four programs put the synonym in a module, because the prototype cannot parse a
*block*-level `pattern` at all — its own recorded divergence
(`values/pattern-synonym-in-block`,
[pattern-synonym-not-a-block-declaration](pattern-synonym-not-a-block-declaration.md)).

| program | prototype | port |
|---|---|---|
| `pub pattern Two(a, b) = Pair(a, b)`, used on `P.Pair(1, True)` | the declaration elaborates; the *use* fails `UnknownConstructor "Two"` (its own defect, [pattern-synonym-arguments-bind-by-position](pattern-synonym-arguments-bind-by-position.md)) | `1` |
| `pub pattern Two(a, b) = (a, b)`, used on `(1, True)` | `ElabError(TupleLengthMismatch)` | `not ported yet: …` |
| `pub pattern Id(x) = x`, used on `1` | `ElabError(NotANominalType)` | `not ported yet: …` |
| `pub pattern IsI64 = I64`, used to type-case a `Type` | `1` | `1` |

The two refusals are fall-throughs rather than decisions: a bare binder is not a nominal
at all, and the product case reports a tuple-arity mismatch when no tuple arity is in
conflict. The fourth row is why this ticket is **not** about type-case right-hand sides:
a zero-parameter type-case synonym already works in both.

## What this changes

- `Elaborator.Patterns.cs:71` — today it refines the scrutinee type from a raw meta and
  then *refuses* when metas remain. The generalization slots in exactly where the
  refusal is: the remaining metas become the synonym's generalized parameters, and the
  stored term carries them for instantiation at a use. This is why the ruling is
  cheaper than the earlier draft — the right-hand side still elaborates **once**.
- A use that cannot solve a generalized parameter is an error naming it, the way an
  unsolved implicit is reported.
- The prototype's `ElabError(TupleLengthMismatch)` (product) and
  `ElabError(NotANominalType)` (bare binder) are **prototype defects** (porting
  convention 5): the shared cases carry the correct `.expect` and are listed in
  `test/conformance/prototype-divergences.txt` naming this ticket.
- **Not settled here.** `Elaborator.Patterns.cs:75` (`NeedsDirectMatch`, a right-hand
  side needing the direct-match machinery) is untouched by this ruling and belongs to
  [the unverified rows](port-unverified-rows.md). `:87` ("parameter types not fixed")
  should simply dissolve, since a parameter's type is exactly what gets generalized.
- **For the implementing fork to report rather than guess** (convention 9), if either
  turns out to need a semantic choice: whether the generalized parameters are *implicit
  type parameters of the synonym* (solvable by a use the way `f[I64]` solves an
  implicit), or rigid variables solved by the use's scrutinee type alone; and what
  happens when a use's scrutinee type is itself a meta (inside an unannotated lambda) —
  that case must either stay stuck until the meta is solved or be reported as unsolved
  where the scope ends, exactly as an unsolved implicit is.

## Tests

Shared cases, run through both runners as the acceptance condition:

1. `values/pattern-synonym-over-product` — `pub pattern Two(a, b) = (a, b)` used on
   `(1, True)`, `expect` `1`. A prototype divergence: list it.
2. `values/pattern-synonym-over-binder` — `pub pattern Id(x) = x` used on `1`,
   `expect` `1`. A prototype divergence: list it.
3. The same declaration used at **two** use sites with different scrutinee types (a pair
   of `I64`, then the pair in the other order), the two results summed, so one
   declaration is instantiated twice and the case cannot pass by accident.
4. A use whose scrutinee type does not fit the right-hand side — the *use* is the error,
   not the declaration.
5. **The check half of the ruling**: an unused synonym whose right-hand side is
   ill-typed with respect to types that *are* known is an error at the declaration —
   e.g. `Pair(a, a)` where `Pair : (I64, Bool)`, so `a` would have to be both.

Prefer a **qualified** use (`M.Two(x, b)`) over `open M` in these cases, so the
prototype's failure stays the one this ticket is about rather than its recorded
open-through-synonym defect.

## Resolution (2026-09-24) — closed

Merged from `pi-agent-2ec360d5-f99f-4ef` (`ee56f2f`, `7c63c48`), on top of the landed
E11 work.

- **What landed.** The two `NotImplementedException` refusals in
  `Elaborator.Patterns.cs` became the `let`-generalization step: `CollectSynonymMetas`
  gathers the unsolved metas of the scrutinee and parameter types; `VPatternSynonym`
  (`dotnet/src/Fun.Kernel/Core.Patterns.cs`) now carries `TypeParams`, `Generalized` and
  the definition's `Env`/`Width`; `InstantiateSynonym` quotes the stored template back
  under the definition environment and substitutes each generalized meta with a fresh
  one, so one declaration is elaborated once and instantiated per use, with captured
  names still the definition's (hygiene unchanged). `ElaborateSynonymUse` and
  `RefineScrutineeType.Implied` instantiate before unifying. `:75`
  (`NeedsDirectMatch`) is untouched and owns
  [its own ticket](port-pattern-synonym-over-type-case-rhs.md); `:87` dissolved, as this
  ticket predicted.
- **Numbers, integrator-verified after merging:** C# conformance **723 → 728, 0
  failed**; xUnit 182/182; `dune test` and `dune test test/conformance` green — 728
  cases, 0 failed, **26** known prototype divergences.
- **Four divergences, not two.** The fork probed instead of assuming, and found the
  prototype also *accepts* an unused ill-typed synonym (`Pair(a, a)` where
  `Pair : (I64, Bool)`), so the "checked" half of the ruling has **no** prototype
  counterpart — the port is stricter, and that case is listed too. The `use-mismatch`
  case is deliberately **not** listed: the prototype's declaration error coincides with
  the coarse `error` expect, so it passes there for the wrong reason. (A green OCaml run
  proves each listed case really does fail there, since the runner reports a listed case
  that passes as a failure.)
- **Which reading of the generalized parameters was taken:** the **rigid-variable** one
  — fresh metas per use, solved by unification against the scrutinee's type — not
  `f[I64]`-style explicit/implicit solving. Nothing in the ruled cases distinguishes the
  two, so this is a choice rather than a defect; if a use should be able to *supply*
  them, that is a new question, not a bug in this work.
- **Left open, untested:** a use whose scrutinee type is itself a meta (inside an
  unannotated lambda). `Implied` instantiates and lets it stay stuck; the port has no
  scope-end unsolved-meta report, and no case exercises it yet.
