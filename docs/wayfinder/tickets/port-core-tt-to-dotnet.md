---
title: Port core_tt to .NET (F#/C#)
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee:
blocked_by:
  - domain-model-core-tt.md
  - domain-model-surface-enforestation.md
  - imported-module-elaboration-context.md
  - env-width-contract-is-unnamed.md
  - unify-primitive-declaration.md
  - constructor-lookup-matches-type-name.md
  - core-traversals-ignore-binding-list-depth.md
  - struct-open-does-not-scope-over-con-fields.md
---

# Port `core_tt` to .NET (F#/C#)

## Question

Keep the `core_tt` language model, reimplement the compiler and runtime on .NET,
target the CLR (GC/JIT/tooling for free) rather than a custom VM, preserving room
for effects via CPS/trampolining. Promoted from the design map's fog list now
that it has a blocking set.

Open within this ticket: **F# or C#**. F# is far closer to the prototype —
discriminated unions, exhaustive matching, immutability by default — so the
elaborator and NbE transliterate with the pattern-match structure intact, and
the compiler still catches a missing case when a `Core.term` variant is added.
C# gets the mainstream tooling and ecosystem. A split (F# core, C# tooling/host)
is a third option. Decide before writing, not during.

## Why it is blocked rather than started

The prototype's load-bearing invariants are not in its code — they are in the
author's head, and a port carries code. See
[domain model for core_tt](domain-model-core-tt.md) for the argument and the
evidence: four separate defects, each one a rule with no name in the source.
Porting first means re-deriving each invariant in a language where the mistakes
are harder to find, and inheriting the defects looking deliberate.

The blockers are therefore of two kinds:

**Model** — [domain model for `core_tt`](domain-model-core-tt.md). The port's
specification. Types in the new implementation should be named after its
vocabulary.

**Defects that would be ported faithfully:**

- [imported modules elaborate in the importer's context](imported-module-elaboration-context.md)
  — a crash today; also the cache design the port would copy
- [env-width contract is unnamed](env-width-contract-is-unnamed.md) — the single
  most likely thing to be silently mis-transcribed
- [one declaration per primitive](unify-primitive-declaration.md) — four
  hand-synced string copies become four in .NET
- [constructor lookup matches the type name](constructor-lookup-matches-type-name.md)
- [core traversals ignore binder depth](core-traversals-ignore-binding-list-depth.md)
  — the env-width invariant broken in a third place
- [struct open does not scope over `con_fields`](struct-open-does-not-scope-over-con-fields.md)
  — settle the rule before the struct elaborator is written a second time

Explicitly **not** blocking: diagnostics polish (deliberately deferred to
post-rewrite), enforester combinator work, the IR-layer-count question — the
last is worth resolving *during* the port design, not before it.

## Related

- [formalized core semantics](../topics/formalized-semantics.md) — a Lean/Coq
  spec as an AI-checked structural-correspondence reference across the port.
  Depends on the same vocabulary the domain-model ticket produces.
- [too many IR layers](../fun-design-map.md#fog) — `Syntax.t` and `Surface.t`
  look near-isomorphic; whether one collapses is a port-design question.

## Resolution

_Unresolved._
