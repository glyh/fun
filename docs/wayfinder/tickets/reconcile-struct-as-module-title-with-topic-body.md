---
title: Reconcile the struct-as-module decision title with its topic body
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
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

_Unresolved._
