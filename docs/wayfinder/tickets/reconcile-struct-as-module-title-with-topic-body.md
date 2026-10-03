---
title: Reconcile the struct-as-module decision title with its topic body
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: closed
assignee:
blocked_by: []
---

# Reconcile the struct-as-module decision title with its topic body

## Question

The design map's decision line says "one `struct` construct serves as record, module,
and namespace," but [struct-as-module](../topics/struct-as-module.md) now opens
"`module` and `struct` are separate constructs… not interchangeable at the type level."
Which is the ruling?

## Context

- The Graydon-constraint audit (2026-10-01) flagged that the decision title contradicts
  the topic body.
- The title records the original unification decision; the body records a later
  refinement (a module is never a type, a record struct is not a module signature). The
  two have drifted apart.
- This is a naming/ruling question, not a code question — no behavior changes either way.

## Resolution

**Reconciled 2026-10-04.** Both statements are true at different levels, and the map line
now says so. The title records the original unification decision: one `struct` construct
serves as record, module, and namespace. The topic body records the later refinement:
`module` and `struct` are separate constructs sharing member syntax, not interchangeable
at the type level — a module is never a type, and a record struct is not a module
signature. No behavior changes either way; the map line was the drift.
