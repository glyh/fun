---
title: Constructor lookup matches the type name, not the constructor name
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
assignee: glyh
blocked_by:
---

# Constructor lookup matches the type name, not the constructor name

## Question

`Elab_resolve.find_nominal_template_opt`'s env scan compares the candidate's
**type** name against the name being looked up:

```ocaml
| VNominal n when String.equal n.name name -> Some (VNominal n)
```

So resolving a *constructor* by name never succeeds on that path. Promoted from
the design map's fog list ("known deferred bug — nested-module ADT constructor
resolution"), where it was salvaged from an old handover note; confirmed still
live in `lib/semantic/typecheck/elab_resolve.ml`.

## Symptom

`pub pattern PatWild = RawPatWild(_)` inside a module fails: the pattern
synonym's right-hand side names a constructor, the path scan falls through to
`scan_env`, and `scan_env` only matches type names. Affects pattern matching in
macro bodies over module-scoped ADTs.

`Elab_resolve.find_nominal_for_constructor` right above it does the correct
thing (scans `n.constructors`), so the fix is likely to route the fallback
through that rather than to write new logic.

## Why it is a pre-rewrite item

It is a plain semantic bug in resolution, not an OCaml artifact — a port
reproduces it faithfully, and it will look deliberate to whoever transcribes
it.

## Sketch of the work

1. Decide whether `find_nominal_template_opt`'s fallback should match type names
   only, constructor names only, or both with a stated precedence.
2. Reuse `find_nominal_for_constructor` for the constructor half.
3. Regression: a module-scoped ADT with a `pub pattern` synonym over one of its
   constructors.

## Resolution

**The premise was stale; the resolution order is now named rather than
re-derived.** Probed against a live elaborator before editing anything.

1. The reported symptom passes today. A `pub pattern` synonym over a
   constructor elaborates in all four shapes tried: same-module ADT,
   nested-module qualified rhs, sibling-module qualified rhs, and the ticket's
   exact `RawPatWild(_)` wildcard-argument shape.
2. `find_nominal_template_opt` comparing `n.name` is not a defect. Looking up a
   *type* by name is that function's job. Both live callers already fell back to
   constructor lookup when it returned `None`.
3. What was actually wrong was that the fallback existed **twice**, written out
   by hand in `elab_resolve.ml` and again in `elab_match.ml`, and that
   `find_nominal_template` — the raising wrapper — had no callers anywhere in
   the project.

So the answer to the ticket's first question, *type names only, constructor
names only, or both with a stated precedence*, is **both, type name first**.
That was already the de facto behaviour in both callers; it just had no name.

Changes:

- `elab_match.ml` now calls `find_nominal_for_pattern_head_opt` for its fallback
  instead of re-inlining it. The type-name hit still short-circuits to `VU`,
  because there a type-name head means the scrutinee is `Type` itself — that
  distinction is why the two copies could not simply be folded into
  `find_nominal_template_opt`.
- `find_nominal_template` deleted as dead.
- The precedence rule is written above `find_nominal_for_pattern_head_opt`.

Regression: `same-module pattern synonym constructor` in
`test/semantic/test_elaborate.ml`, covering the shape the ticket claimed was
broken. Full suite green, 810 tests.

### Follow-on, also fixed here: `type T = T I64`

An ADT whose constructor shares its type's written name failed two different
ways, and neither was a namespace question. Both were one rule — *a later
binding shadows an earlier one* — implemented inconsistently.

**Qualified (`M.T(7)` raised `ApplyingNonFunction`).** A declaration emits the
module entries `T` (the type), `T` (the constructor), `Y`, in that order. Every
dotted field lookup took the **first** match, so `M.T` denoted the type and
applying it failed. Meanwhile `open M; T(7)` built the constructor correctly, and
`do x = 1; x = 2; x` evaluates to `2`. Qualified lookup was the only resolution
path in the language that ran first-wins.

The rule was written out seven times across two libraries as a bare
`List.find_opt`, in `elab_resolve.ml`, `elab_infer.ml` and `nbe_support.ml` — the
elaborator and the evaluator each deciding independently what a path denotes.
It is now `Core.find_field_last`, named and commented in `core.ml`, used by all
seven. Same family as
[env-width-contract-is-unnamed](env-width-contract-is-unnamed.md).

**Unqualified (`Failure "ambiguous binding for T: scopes {1} and {0}"`).** In
`expand.ml` the type name and each constructor were bound with sibling scope
sets, both fresh from the enclosing scope. Set-of-scopes resolution needs one
candidate to be a subset of the other; incomparable siblings are an ambiguity
error. Ordinary shadowing avoids this because a later `do` binding's base context
already contains the earlier one's.

Constructors are now bound under the type's scope rather than beside it, at both
`TypeDef` and `TypeBinding` sites, making the constructor strictly more specific.
Constructors remain siblings of each other, so `type T = X | X` is unchanged.

**The tradeoff, stated.** Under one namespace with last-wins, the constructor
shadows the type, so after `type T = T I64` the name `T` no longer works in type
position: `fn(x: T) -> …` fails to unify. The collision costs you one of the two
names either way; before this change it cost the constructor, which was worse,
because the type was still nameable but the ADT could not be built. Giving both
names back means separating the type and value namespaces, which contradicts
*types are values* and is a language-level decision, not a repair.

Regressions in `test/backend/test_core.ml`: `constructor sharing its type name`,
`qualified constructor sharing its type name`, and `duplicate module field
resolves to last`. Full suite green, 813 tests.
