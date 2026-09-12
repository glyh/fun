---
title: Impls and traits extend the context outside the slot list
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
assignee: glyh
resolution: Every module and struct binding now extends the context through one helper driven by Core.binding_slots, impls and traits included. An impl's elaboration was split into what it contributes and the evidence that rides along with it, so the binding fold takes the entry rather than receiving a context someone else extended. Both binding-list width checks and the BindingWidthDrift error are gone.
closed_date: 2026-09-12
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


## Resolution

`extend_from_slots` is the one place a module or struct binding extends the
elaboration context: it reads the slots of the term the binding emits and folds
the elaborator's payloads — a type and a value per slot — onto them. Every case
in both folds goes through it, so a disagreement between the term a binding
emits and the entries it pushes fails while the context is being built.

Impls needed a split to get there. `elaborate_impl_contribution` works out what
an impl contributes — the dictionary type and value that occupy its single entry,
plus the trait identity — without touching the context, and
`install_impl_evidence` adds the furniture that rides along but adds no entry:
the evidence resolution searches, and the optional name an `impl NAME : …` is
reachable by. `elaborate_impl` is those two composed, for impls in expression
position where there is no binding term and so no slot list.

Traits turned out never to have been an exception: `elaborate_trait` does not
extend the context at all, and its caller defines the one entry.

With every kind routed, both binding-list width checks and the
`BindingWidthDrift` error are deleted — there is nothing left for them to
compare. Verified load-bearing: passing one extra payload for an effect binding
fails the suite at once. 842 tests green.
