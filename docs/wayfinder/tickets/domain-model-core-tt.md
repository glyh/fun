---
title: Domain model for core_tt before the port
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee: glyh
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

**First pass done.** Model in
[core-tt-domain-model](../topics/core-tt-domain-model.md), vocabulary in the root
[`CONTEXT.md`](../../../CONTEXT.md). Five invariants named and classified, one
defect found and fixed, one dead concept deleted.

The pass corrected the ticket's own framing. `Elab_ctx.Ctx` and the NbE `env` are
not "two views of one thing": the evaluator holds **one column of** the
elaborator's scope and never sees the rest. No operation on a scope uses all of
it — evaluation takes the value column, quoting takes the width, meta creation
takes the bound/defined mask. Modelling them as peers is precisely the mistake a
port would have made.

A fifth column, one type per entry, turned out to be **dead**: written by every
constructor, read only by the code that rebuilt it. Types live in the name table.
Deleted, so the port does not inherit a parallel list it must keep correct for
no reason.

One defect fell out: elaborating a parameterised ADT extended the value column
and the width but not the mask, breaking the equal-length invariant every other
site maintains. Latent — it surfaces only as `bd mask length mismatch`, and no
program in the suite triggers it. Fixed. Suite green, 816 tests.

### Where the model is weakest

There is still **no notion of a closed term** — one whose indices refer to
nothing outside itself. Nothing distinguishes a term safe to cache or to carry
across a module boundary from one that is not, which is the unnamed rule behind
[imported-module-elaboration-context](imported-module-elaboration-context.md).
Naming it is the highest-value item in the next pass, and that ticket is probably
unfixable-by-design until it exists.

### The fourth bullet, finished

*Which names exist and which namespace each lives in* turned out to be two
questions, and the answer is two-level. **Bare names** — values, types,
constructors, pattern synonyms, effect families, traits, modules — share one
namespace and shadow each other. **Members** — record fields, module members,
effect operations, trait methods — are reached only through a container and never
compete with a bare name. A record field named `x` does not shadow a value named
`x`; verified, along with the other four rows.

That is why last-wins path lookup and last-wins shadowing are two rules rather
than one: they govern different namespaces.

**The single-namespace rule does not survive the phase boundary.** Whether a name
is an operator or a syntax form is settled by the expander, string-keyed and
newest-wins, before scope sets exist. So a later binding cannot take the role
away — `do not = 5; not end` is a *parse* error, and so is `do if = 5; if end`,
while the parenthesised `(+)` rebinds fine. `binding.ml` documents this and
defers scope-keyed operator resolution to the interleaving work. A name therefore
has a **syntactic role** as well as a scope entry, and the two resolve by
different rules.

### Remaining passes (not this ticket)

Surface and enforestation; macro expansion and hygiene; effects and handlers.
The syntactic-role seam above is the way into the hygiene pass, and the
elaborator holding both its own macro table and a mutable reference to the
expander's context is the same coupling from the other side.
