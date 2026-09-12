---
title: The elaborator's expander handle is named as a context but used as a capability
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
assignee: glyh
resolution: Elab_ctx.Ctx now holds a macro_runtime — how to run a macro, and the expansion-depth budget to run it under — instead of a borrowed Expand_ctx.t. The adapter that narrows an expander to those two capabilities is the only place the elaborator sees the expander at all.
closed_date: 2026-09-12
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


## Resolution

`Elab_ctx.Ctx.expand_ctx` is now `macro_runtime`, a record of the two
capabilities the elaborator actually uses: `run_macro`, how to apply a macro
value, and `with_fuel`, the expansion-depth budget to apply it under. The
elaborator no longer holds a reference to the expander, and the type says so.

`Ctx.macro_runtime_of_expander` narrows an expander to those two and is the only
place the two libraries meet on this path. It returns `None` when the expander
cannot run a macro at all, which collapses the two nested "is it there" matches
at each call site into one.

842 tests green.
