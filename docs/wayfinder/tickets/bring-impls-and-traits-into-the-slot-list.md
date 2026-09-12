---
title: Impls and traits extend the context outside the slot list
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Impls and traits extend the context outside the slot list

## Question

`Core.binding_slots` is the one statement of what a binding contributes to a
context: an ordered list, one slot per entry, consumed by the evaluator to push
values and by the elaborator to hang a type and a value on each. Two binding
kinds do not go through it.

An impl's context extension happens inside `elaborate_impl`, and a trait's inside
`elaborate_trait`, each returning a context they extended themselves. Both are
one entry today, which is why nothing has drifted, but the count lives in those
functions rather than in the slot list, and the binding-list width check is what
stands between a disagreement and a wrong de Bruijn index.

## Why it matters for the port

The slot list exists so the port inherits one description of order and count
rather than three implementations of it. Two kinds outside it means the port
inherits the exception as well, in a language where the check that currently
covers them may be written differently or not at all.

## Shape of the work

Have `elaborate_impl` and `elaborate_trait` return their contributed entries
rather than an extended context, and let the caller fold them onto the slot list
the way `elab_type_binding` now does. The binding-list check then has nothing
left to cover and can go with them.

Small, but not mechanical: both functions use the context they build while they
are still building it.
