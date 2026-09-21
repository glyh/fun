---
title: "Port: implicit application — f{ e } and applying a value of unknown function type"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-20
resolution: Both gaps ported by mirroring the prototype's own rules (enforest.ml:636-647; elab_apply.ml:158-175), with six cases that pin behaviour rather than just elaborating. Found and split out one divergence: an inline vs named polymorphic lambda, port-generalise-under-check.
assignee:
blocked_by:
---

# Port: implicit application

The two **verified** real gaps from [the unported-path audit](port-unported-path-audit.md)
(section "Real gaps", G1) — the only two the audit reproduced in *both* runners, and
the integrator reproduced them again independently. Both are small and self-contained;
either is a good next implementation fork. Follow the
[porting conventions](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16) ·
convention 7 puts the work in its own partial file.

## 1. An implicit argument written `f{ e }`

`dotnet/src/Fun.Expand/Enforest.cs:387` refuses it:

```
throw new NotImplementedException("not ported yet: an implicit argument written f{ e }");
```

Reproduced (2026-09-20, both runners, case `expect` `1`):

```fun
{ f = fn[A : Type](x : A) { x }; f{I64}(1) }
```

Prototype: `1`. Port: `not ported yet: an implicit argument written f{ e }`.

Note the guard at that site already requires adjacency (`lhs.Span.End == postfix.Span.Start`)
and distinguishes `f{ e }` from `f { … }` (a brace group), so the shape is understood —
only the production is missing. The prototype reads it in `lib/expand/enforest.ml`.

## 2. Applying a value of unknown function type

`dotnet/src/Fun.Compiler/Elaborator.Implicits.cs:50` refuses it:

```
throw new NotImplementedException("not ported yet: applying a value of unknown function type");
```

Reproduced (2026-09-20, both runners, case `expect` `1`):

```fun
{ h = fn(g) { g[I64] }; 1 }
```

Prototype: `1`. Port: `not ported yet: applying a value of unknown function type`.

This is the implicit-argument analogue of a decision already taken for references
([a reference operation on a value of unknown type is "not a reference"](deref-of-unknown-type-is-not-a-reference.md),
closed 2026-09-20): the operation *infers* rather than refusing. So this should infer
the implicit function type and insert the meta, exactly as ordinary application infers
an arrow — check that reading against the prototype before implementing, and if it
disagrees, stop and report rather than guessing (convention 9).

## Tests

Both are shared conformance cases (convention 6 — a source-to-result test is a
conformance case). Use the programs above, `expect` `1`. Run them through both
runners before committing: the prototype is known to pass both (verified 2026-09-20),
so if one fails in the prototype that is a new finding, not a case to adjust.

## Resolution (2026-09-20) — both gaps closed

Merged from `port/implicit-application` (`8845cfe`, `f335ea2`). **C# conformance 707 →
713 cases, 0 failed**; xUnit 178/178 unchanged; `dune test` and
`dune test test/conformance` green (713 cases, 20 divergences).

**1. `f{ e }`.** The prototype's rule is `lib/expand/enforest.ml:636-647`: after
`require_adjacent_postfix`, a brace group containing `=` is a `RecordConstruct`,
otherwise the group is read as one expression and applied with `Explicitness.Implicit`.
Mirrored as one branch in `Enforest.cs` calling `ParseBraceImplicitApplication`, plus the
reading in `Enforest.Implicits.cs`; a non-adjacent brace group is the adjacency error.

**2. Applying a value of unknown function type.** `lib/semantic/typecheck/elab_apply.ml:158-175`
— a fresh meta domain, the argument checked against it, a fresh meta codomain under the
binder, an `Implicit` `VPi` unified with the function's type. Mirrored line-for-line from
the existing `InferApUnknown` as `InferApImplicitUnknown`. **The ticket's inference
reading held**, verified against the prototype's source before writing code.

**Six cases, all agreeing in both runners**, and deliberately not just the ticket's two —
which would have passed for the wrong reason (the ticket's second program never *uses*
`h`, and the first returns the argument, which any treatment gives):
`implicit-argument-braces` (1) and `implicit-argument-braces-value` (7 — the result *is*
the implicit value, so a dropped brace argument cannot pass),
`implicit-argument-braces-not-adjacent` (error), `apply-unknown-implicit-function` (1),
`apply-unknown-implicit-function-inferred-arrow` (7 — an *explicit* inferred arrow
rejects the poly lambda, verified in both), and `implicit-application-on-unknown-function`
(7 — brace implicit on an unknown type then explicit application, result observed).

**Found, not fixed:** an inline polymorphic lambda and the same lambda bound to a name
diverge, in *opposite* directions between the implementations. Verified by the integrator
and ruled (both must be accepted — they differ only by let-inlining):
[generalizing the argument to an unknown-typed function under a check](port-generalise-under-check.md).
