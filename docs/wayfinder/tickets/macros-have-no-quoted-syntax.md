---
title: Macros have no quoted syntax
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Macros have no quoted syntax

## Question

How does a procedural macro write an id that resolves where the macro was
*defined*? In sets of scopes (Flatt 2016, §4) that is `quote-syntax`: the id
keeps the scopes of its definition site, pruned of the binding forms enclosing
the quote. An id built from a bare name — `datum->syntax` with no context — has
an empty scope set and is unbound.

`fun` has neither half. The only way to build an id is from a string:
`Syntax.var(name)` is `RawVar(None, new_id(name))` (`elab_prelude.ml:149`), an
empty scope set. Such an id cannot resolve in the first tier, so the
implementation added a second one — an unresolved id keeps its written name and
the elaborator resolves it by spelling (the surface model's S6). That tier is
what lets `Syntax.var("True")` reach the prelude, and what makes every
macro-written name capturable by whatever binder of that spelling is in scope.

## What the model asks for

Vocabulary is in [`CONTEXT.md`](../../../CONTEXT.md): **Quoted syntax** and
**Borrowed context**.

- Quoted syntax: a macro-body form whose ids carry definition-site scopes.
- Borrowed context: build an id from a name plus a syntax object whose scope
  set it takes.
- A context-less id has an empty scope set; resolution fails loudly rather than
  falling through by spelling.
- The string-keyed second resolution tier is removed.

Deleting the tier breaks every existing macro that writes a prelude name by
string; they must move to quoted syntax or borrowed context in the same change.

## Progress (2026-09-14)

`quote(…)` is implemented. It is parsed where it is written, and its ids keep
the definition site's scopes, pruned of the macro's own binding forms. It
elaborates to its reflection value, with each hole checked against the type
its position gives it, and evaluation fills the holes. A procedural macro no
longer needs string-built ids for names it means.

**The second resolution tier is gone for bare-name expressions (2026-09-14).**
Expansion resolves every bare name to its binder's resolved name or to an
**open choice**: the candidate opens, chosen by scope set, and the binder they
shadow. The elaborator settles a choice against the opens' members, and never
looks a name up by spelling among the locals. A context-less id reaches only
the base context. The rest of the tier is still to go: path heads (`M.x`,
pattern heads), trait names and effect names are still located by spelling in
the elaborator. They go with delete-surface-ir, when the elaborator reads ids.
Macro bodies still get the prelude open implicitly; expansion mirrors that open
until M3 lands.

## Decided (2026-09-14)

See [M9–M11](../topics/core-tt-domain-model-macros.md). Spelling is
`quote(…)` with `$e` holes; it is parsed at the definition; hole kinds are the
reflection types; `Id.scope` is an opaque `Scopes`; borrowed context is
building an `Id` with another id's `scope` — no separate API. Templates
desugar to macros whose body is the replacement quoted, so this ticket and
[template-literals-resolve-at-use-site](template-literals-resolve-at-use-site.md)
are one change.

## Related

- [procedural-macros-capture-use-site-variables](procedural-macros-capture-use-site-variables.md)
  — spliced args lose scopes at the value boundary; the other half of the same
  missing machinery.
- [block-local-macros-leak-by-written-name](block-local-macros-leak-by-written-name.md)
  — the macro-table side of the spelling tier.
- [template-literals-resolve-at-use-site](template-literals-resolve-at-use-site.md)
  — a template's literals are quoted syntax in the model, and should resolve at
  the definition site for the same reason.
