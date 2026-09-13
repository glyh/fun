---
title: Domain model — surface and enforestation
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: closed
assignee: glyh
resolution: Second pass done — model in ../topics/core-tt-domain-model-surface.md, vocabulary in the root CONTEXT.md. Seven invariants named and classified (S1–S7); the three macro paths measured against the model's one hygiene contract, every divergence a defect ticket (two found or diagnosed this pass, two placed from the IR-layers research). Surface.t is an erasure plus one escape hatch, not a language level; no seam exists in the model — the implementation's fixed passes are the defect. The Self naming question was deferred here and stays fog; the third pass (macro evaluation and hygiene) is claimed.
closed_date: 2026-09-13
blocked_by:
---

# Domain model — surface and enforestation

## Question

Second pass of the domain model: the layers before elaboration. The port carries
the phase boundary between the reader, the enforester and the elaborator, and
today that boundary is described by struct maps and one comment in
`binding.ml`:

- What is a **form**? What does enforestation turn a token list into, and what
  does its result type (`Syntax.t`) promise?
- What is a **scope set** attached to, what mints them, and what does hygiene
  guarantee about a macro-written identifier — stated as an invariant, not as
  Racket folklore?
- What is `Raw_syntax` *for*, given everything downstream speaks `Syntax.t`?
- Is `Surface.t` a distinct concept from `Syntax.t`, or the same thing after
  expansion? The fog item suspects isomorphism; this pass names what differs,
  and the merge decision stays fog.
- What is the **contract at the seam**: what does the expander hand the
  elaborator, and what can the elaborator no longer see?

## Why this is not tidying

Pass one found its defects at the elaborate ↔ evaluate boundary. This pass sits
where the same class lives on the other side:

- I4c — the syntactic role is string-keyed and newest-wins; the rule survives
  only as a comment.
- The elaborator historically held both its own macro table and a borrowed
  expander context (I4e); what remains of that coupling should be named, not
  inherited.
- Hygiene is the port's highest-risk transliteration: a port that copies
  scope-set threading without the invariant passes every test until the first
  macro that writes a binder.

## Scope

The reader (`Raw_syntax`), enforestation (`Syntax.t`, `Enforest` and the
form/pattern/template helpers), scope sets as carried by `Syntax.t`, expansion
(`Expand`, `Expand_ctx`, `Macro_eval`'s surface face), and lowering
(`Lower_surface`, `Surface_to_syntax`).

Not this pass: the hygiene *resolution rule* and macro evaluation semantics
(third pass — [domain-model-macro-hygiene](domain-model-macro-hygiene.md)),
effects and handlers (fourth pass —
[done](../topics/core-tt-domain-model-effects.md)), and the IR-collapse merge
decision — this pass supplies that investigation's vocabulary and stays
read-only about merging.

## Deliverables

1. A topic doc under `topics/` naming the concepts and invariants of each
   pre-elaboration layer, each marked enforced / checked / unchecked.
2. `CONTEXT.md` vocabulary for those layers.
3. Defects and misnamed concepts spun out as their own tickets.

## Resolution

**Done.** Model in
[core-tt-domain-model-surface](../topics/core-tt-domain-model-surface.md),
vocabulary in the root [`CONTEXT.md`](../../../CONTEXT.md). Seven invariants
named and classified (S1–S7); the three macro paths measured against the
model's one hygiene contract, and every divergence is a defect ticket.

The pass corrected the ticket's own framing in three places:

- **`Surface.t` is not a language level.** The ticket asked whether it is a
  distinct concept from `Syntax.t`; the answer is that it is an *erasure* of
  one — spans, scope sets, macro kinds and operator units stripped, nothing
  else — plus a single escape hatch (`StxExpr`, unstripped syntax smuggled past
  lowering for the reflection ADTs). The merge decision stays with
  [delete-surface-ir](delete-surface-ir.md), as scoped.
- **There is no seam in the model.** Expansion and elaboration interleave
  binding by binding; a hand-off tree is an artefact of the implementation's
  fixed passes (enforest → expand → lower → elaborate). That the passes are
  fixed at all is recorded as a defect of the model, not a phase boundary to
  port.
- **The implementation has no single hygiene invariant.** It has three, one
  per path, of which only untyped heads are both hygienic and tested. The
  model's one contract is settled — two fresh scopes per application, quoted
  ids resolving at the definition site, splices keeping their scopes, output
  expanded in place — and the distance to it is the four defect tickets, not an
  open design question.

Defects found or placed by the pass:

- [template-literals-resolve-at-use-site](template-literals-resolve-at-use-site.md)
  — found here: a use-site `False = 42` silently turns the prelude's own `&&`
  into a constant-42 machine.
- [procedural-macros-capture-use-site-variables](procedural-macros-capture-use-site-variables.md)
  — diagnosed here: scope sets die at the macro value boundary. The third-pass
  ticket sharpened the diagnosis while being grounded: they die at *both* ends
  of the round trip.
- [type-aware-macro-output-is-not-expanded](type-aware-macro-output-is-not-expanded.md)
  and [block-local-macros-leak-by-written-name](block-local-macros-leak-by-written-name.md)
  — found by the IR-layers research; this pass placed them (S5's skipped-expand
  seam, S6's fall-through twin).

Deliberately not taken: the `Self` naming question (deferred here by the fog
list; it remains fog under
[self-type-has-no-identity](self-type-has-no-identity.md)), the IR merge, and
the hygiene resolution rule with macro evaluation semantics — now claimed as
the third pass,
[domain-model-macro-hygiene](domain-model-macro-hygiene.md). Effects were the
fourth pass and are done
([core-tt-domain-model-effects](../topics/core-tt-domain-model-effects.md)).
