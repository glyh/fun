---
title: The elaborator's expander handle is named as a context but used as a capability
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# The elaborator's expander handle is named as a context but used as a capability

## Question

`Elab_ctx.Ctx.expand_ctx` reads as "the expander's context", which suggests the
elaborator consults the expander's binding table. It does not. The field is read
for exactly two things: `eval_and_apply`, which is how to run a macro, and
`Expand_ctx.with_macro_fuel`, an expansion-depth budget. Both are capabilities;
neither is a namespace.

Recorded as I4e in
[core-tt-domain-model](../topics/core-tt-domain-model.md). It is the same shape
as the mistake I1 corrects: a name promising a whole context while delivering one
projection of it.

## What has already been fixed

The latch is gone. The field used to be reassigned by the macro driver and again
by each `Import`, never restored, so the last writer won for the rest of
elaboration. That mutation was deleted in
[base-context-shared-state](base-context-shared-state.md), where it turned out
never to have been load-bearing.

## What is left

The name, and the width of what it hands over. Give the elaborator the two
capabilities it actually uses rather than a borrowed expander, so the port does
not model a dependency that is not there.

Worth doing before the port rather than after: it is a rename plus two call
sites, and it removes a link between two libraries that a port would otherwise
reproduce as a real coupling.
