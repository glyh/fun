---
title: "Port: latent form gaps — traversals, the reflection reader, operator macros, rec-enum captures"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-20
resolution: G2 closed by making Term.Map total with a reflection test that fails on any new kind; G3's UnitTok and param trait bounds enabled. The rest are model changes (the reflected Syntax ADT differs) or unverified reachability (G4, G5) - spun out. G6 ruled separately.
assignee:
blocked_by:
---

# Port: latent form gaps

The real gaps from [the unported-path audit](port-unported-path-audit.md) (sections
G2–G6) that no shared case reaches — that is what makes them *latent*: the port's
refusal is invisible to the conformance runner until something exercises the form.
Read the audit's own sections for the probe detail; this ticket is the work list and
the ordering.

Every item here has the same required first step, so it is worth stating once:
**write the shared case that reaches the form, run it through both runners, and only
then port.** For a latent gap the prototype is the only evidence that the form is
meant to exist, and the case is the thing that stops it going latent again
(convention 6). If the prototype refuses it too, the item becomes a
[parity conversion](port-parity-conversions.md) instead, and the case is `expect error`.

## G2. Traversals that miss a whole form (broadest reach — do this first)

One defect wearing several sites: a traversal that does not cover every kind of the
thing it walks, so any feature that can contain that form cannot be walked at all.

- `Fun.Kernel/Core.Shift.cs:67` misses six `Term` kinds; `:107` misses `BindingTerm.Impl`
- `Fun.Compiler/Elaborator.Enum.cs:295`, `:314`, `:352` (a form whose names/variables are unknown)
- `Fun.Compiler/Unify.cs:206`
- `Fun.Compiler/Nbe.StuckMatch.cs:50`

Prototype reference: `Core.map_subterms` (`lib/core_kernel/core.ml:618`) — the single
traversal the prototype reads. **This is the same lesson as
[each core-term traversal counts binders on its own](core-traversals-count-binders-separately.md)**
(closed, prototype side): one traversal, not N hand-written scans. Prefer making the
C# side single-sourced over adding one `case` per site — a missing `case` here is a
symptom of the duplication, so fixing the six symptoms without the cause leaves the
next `Term` kind with the same bug.

## G3. The reflection reader refuses forms the prototype reads

`Fun.Compiler/Reflection.cs:603`, `:718`, `:922`, `:726`, `:928`, `:762`, `:850` —
a reflected `RawTypeDef`, bounded params, a unit token, and multi-argument traits and
impls. Prototype: `lib/expand/macro_eval.ml:494`, `:593`, `:603`, `:610`, `:893`, `:898`.

Consequence if unported: a macro whose output mentions one of those forms cannot be
read back, so it fails as "not ported yet" rather than as a macro error. Note the
reflection contract from the macro domain model — reflection is **total** and the
round trip is the identity — so this is not a nicety: each refusal is a hole in a
total function. `Reflection.cs:718/922` and `:726/928` are the same pair of defects
duplicated across two paths; fix both.

## G4. A type-aware operator macro

`Fun.Expand/Expander.Macros.cs:306`. Related to the operator-macro work that landed
with [unit interleaving and operator macros](port-unit-interleaving.md) (closed), which
covered the untyped case. The prelude in `dotnet/std/stage2.fun` declares operators
already, so check whether a *typed* operator macro exists in the prototype at all
before porting — if only the untyped form is exercised, this may be parity.

## G5. Recursive-enum captures predicted by name

`Fun.Compiler/Elaborator.RecTypes.cs:62` — "a recursive enum whose payload types
capture a variable its body does not name". The prototype predicts captures by the
enclosing module's *use*, not by the payload's spelling
([nominal identity is applicative by purity](nominal-identity-applicative-by-purity.md),
"Grilled (2026-09-15), part 1"), so a name-based prediction both over- and
under-captures. Overlaps [port-nominal-identity](port-nominal-identity.md) — check
whether that fork's captures work subsumes this before starting; if it does, close
this as a duplicate rather than adding a second capture rule.

## G6. A stuck match on a known scrutinee's unknown part

`Fun.Compiler/Nbe.StuckMatch.cs:12` (`:50`, `:54` follow from it): the port waits only
on an unknown *scrutinee*, where an unknown **part** a pattern inspects should also
wait. **Decided 2026-09-20 by the user: the match waits — it does not take the default
arm — so the port is right and the prototype is wrong.** That makes it a recorded
divergence rather than parity. Implemented on its own ticket:
[port-stuck-match-sub-occurrence](port-stuck-match-sub-occurrence.md), which carries
the prototype's rule (`lib/backend/interp/nbe.ml:784-810`), the refusal sites, and the
warning that the shared case must be *demanded by the checker* to be observable at all.

## Internals parity — settled

[The audit's ruling](port-unported-path-audit.md#ruling-user-2026-09-20-behavioural-only--budget-yes-shapes-no)
(2026-09-20): source → result parity, plus a `BudgetTests.cs` for the three observable
budget cases. The shape/scope-set/parser-combinator suites are explicitly **not**
mirrored, so nothing in G2–G6 needs a shape assertion — the source → result case is
the contract.

## Resolution (2026-09-20) — G2 and G3

Merged from `port/latent-form-gaps-g2-g3` (`1edd7af`, commits `e95b5ff`, `11ba255`,
`d7f8371`, `1a4b30e`). **C# conformance 695 → 707 cases, 0 failed; xUnit 172 → 178**;
`dune test` and `dune test test/conformance` green.

### G2 — one traversal, made total, with its exhaustiveness *tested*

C# has no exhaustiveness check for an open record hierarchy (`CS8509` fires on any
hierarchy switch without a catch-all, so `WarningsAsErrors=CS8509` buys nothing here).
So the prototype's `Core.map_subterms` answer was taken and the gap closed by test
rather than by compiler:

- `Term.Map` is complete — `Tunnel`, `RecursiveOccurrence`, `Sig`, `TraitRef`,
  `TraitDictTy`, and a handler `Match`'s effect branches were the missing kinds.
- New `dotnet/test/Fun.Tests/CoreTraversalTests.cs` **enumerates every kind by
  reflection**, constructs one and walks it, for `Term`, `BindingTerm`, `Syntax`,
  `Binding` and `Pattern`. A new kind fails there until it is walked — verified by
  deleting a case and watching the test fail, and the sample-builder documents its own
  failure mode (a kind with a field the traversal dereferences but the builder cannot
  construct fails rather than passing silently).
- Pattern binder counts are now **one function**, `CorePattern.Binders()`, read by both
  `Core.Shift.ArmBinders` (which also now reads `DecisionTree.Sequential`) and
  `Nbe.StuckMatch`; `MapBindings` dispatches per binding.
- `Elaborator.Enum.NamedLevels` reads `Syntax.Map` and `FreeLevels` reads `Term.Map`, so
  the free-name and free-level walks are single-sourced rather than hand-written.
- `Unify.Rename` is complete for every `Value` kind the prototype's `rename` handles
  (`VModule`, `VStruct` bindings, `VRecord`, `VSig`, `VTrait`, `VTraitDict`, `VFix`,
  `VGlued`), keeps a deferred call a call, and refuses `VRef`/`VCont`/`VPatternSynonym`
  with `UnifyException` where the prototype has `CannotUnify`.

**Not done, and named:** `Rename`/`FreeLevels`/`NamedLevels` are now *readers* of `Map`
rather than folded into `map_subterms` itself. That is the remaining unification, and it
is small — the hand-written scans are gone, which was the defect; the last step is
cosmetic.

### G3 — enabled, and the rest is a model change, not a reader fix

- **`UnitTok`** — reads as the empty paren group the same source reads as; shared case
  `macros/port-reflected-unit-token` (`expect` `1`, prototype agrees).
- **A parameter's trait bounds** — `Param.Bounds` added with its reader, writer and
  mapper. No shared case can observe it: the elaborator reads source bounds from the
  type (`TraitBoundSet`), so it is covered by the xUnit round trip
  `AParameterWithTraitBoundsRoundTrips`.
- **Multi-parameter traits / multi-argument impls and a reflected `RawTypeDef` are not
  reader fixes at all** — they are places where the port's reflected **Syntax ADT
  differs** from the prototype's (one `Id Param` where a list is needed; no `TypeDef`
  node at all, since stage 2 desugars it). Since reflection is total and the round trip
  the identity, the ADT a macro sees is part of the language contract. Split out:
  [the reflected Syntax ADT differs](port-reflected-adt-differs.md), which needs a ruling
  before a fork.

### G4, G5, G6 — reported, not implemented

- **G4** (a type-aware operator macro, `Expander.Macros.cs:306`): it refuses
  `entry.Signature is not null`, which the prototype's `syntax_operator_arg` admits. No
  program constructed, so reachability is unverified — same shape as everything else
  here: the case comes first.
- **G5 is *not* subsumed by E11**, checked rather than assumed: `PredictCaptures` still
  predicts from `NamedLevels` only while `InferEnum` adds payload `FreeLevels`, and
  `CompletePending` still throws on mismatch — so a name-based prediction beside a
  use-based one is still there. But **no triggering program was found** (a block-local
  probe passed in both runners), so reachability is unverified. Do not "fix" it on
  inspection alone: either construct the program or close it as unreachable.
- **G6** is ruled and has its own ticket: [a stuck match waits](port-stuck-match-sub-occurrence.md).
  The port is right, the prototype takes the default arm.
