---
title: Domain model for core_tt before the port
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee:
blocked_by:
---

# Domain model for `core_tt` before the port

## Question

Build an explicit domain model — a ubiquitous language — for `core_tt` and the
pipeline around it, so the .NET port has a specification to port *to* rather
than an OCaml codebase to transliterate.

## Why this is not tidying

A port carries **code**. It does not carry invariants that are not in the code.
Every defect currently blocking the port is the same shape — a rule that lives
in the designer's head and has no name in the source:

| Rule with no name | Consequence |
|---|---|
| how much scope a binding contributes | [env-width contract](env-width-contract-is-unnamed.md): two libraries do the arithmetic independently, unchecked |
| what context a module's core term is relative to — there is no notion of a *closed* module term | [importer-context bug](imported-module-elaboration-context.md): a crash on the second import |
| a primitive as one thing (name + type + reducer) | [prim declaration](unify-primitive-declaration.md): identity replicated across four places by bare string |
| constructor name vs type name | [constructor lookup](constructor-lookup-matches-type-name.md): one comparison conflates them |

These were not found by reading for style. Each was found by tripping over it.
Transliterating the code reproduces all four, and in a language with a different
evaluation and exception model they are harder to find a second time.

## Scope of the first pass

The **elaborate ↔ evaluate boundary**, where three of the four live:

- What is a binding, and what does each kind contribute to scope?
- What is a term closed *with respect to*? What are the well-formedness
  conditions on a `Core.term` crossing a module boundary or a cache?
- What is the contract between `Elab_ctx.Ctx` and the NbE `env` — they are two
  views of one thing and are never described as such.
- Which names exist: type / constructor / field / macro / operator / effect
  operation — and which namespace each lives in.

Later passes (not this ticket): surface and enforestation, macro expansion and
hygiene, effects and handlers.

## Deliverables

1. A domain-model document under `docs/wayfinder/topics/` naming the concepts
   and their invariants — the terms the port's types should be named after.
2. Each invariant marked as *enforced by construction*, *asserted*, or
   *unchecked convention*, with the unchecked ones listed as work.
3. Ticket updates where the modelling shows a current name is wrong.

Feeds directly into
[formalized core semantics](../topics/formalized-semantics.md) — a Lean/Coq
spec needs exactly this vocabulary first.

## Resolution

_Unresolved._
