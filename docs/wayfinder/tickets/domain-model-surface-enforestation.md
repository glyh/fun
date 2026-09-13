---
title: Domain model — surface and enforestation
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee: glyh
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
