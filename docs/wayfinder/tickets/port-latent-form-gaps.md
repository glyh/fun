---
title: "Port: latent form gaps — traversals, the reflection reader, operator macros, rec-enum captures"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
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
on an unknown *scrutinee*, where the prototype also waits when a known scrutinee has an
unknown *part* a pattern tests. **Undecided in the audit** — `Nbe.Match.cs:87` asks
whether a neutral sub-occurrence should take the default arm (the prototype) or make
the match stuck, and the two readings are not obviously the same rule. Get a ruling
from the user with a concrete program before implementing; do not infer it from
"the prototype does X" alone, since the marker under an argument performing a read is
exactly the area where the prototype has its own stopgap
(`Elaborator.Effects.cs:304`, "a codomain depending on a performing argument reads the
stand-in").

## Internals parity — settled

[The audit's ruling](port-unported-path-audit.md#ruling-user-2026-09-20-behavioural-only--budget-yes-shapes-no)
(2026-09-20): source → result parity, plus a `BudgetTests.cs` for the three observable
budget cases. The shape/scope-set/parser-combinator suites are explicitly **not**
mirrored, so nothing in G2–G6 needs a shape assertion — the source → result case is
the contract.
