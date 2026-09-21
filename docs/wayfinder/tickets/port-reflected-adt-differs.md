---
title: "Port: the reflected Syntax ADT differs — a trait with several parameters, an impl with several arguments"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-20
resolution: Ruled by the integrator on evidence (no trait in the language takes two parameters; every one of the 707 cases, the prelude and std use Eq(A)/Show(A)/Size(A)/Same(A)). Both halves are now **deliberate narrowings** of the reflected ADT. Trait arity is a divergence that must give a named macro error. The TypeDef node is **not** ported either - ruled (user, 2026-09-20), because `type` is a macro in both implementations, so a node for it in the macro-visible ADT is meaningless; see port-reflected-typedef.
assignee:
blocked_by:
---

# Port: the reflected Syntax ADT differs

Two **model-level** refusals found by [the latent gaps](port-latent-form-gaps.md) fork
in the reflection reader, both confirmed by probe (prototype `1`, port refuses):

- `Reflection.cs:718`/`:922` and `:726`/`:928` — a trait with more than one parameter, an
  impl with more than one argument (`not ported yet: a trait with other than one parameter`).
- `Reflection.cs:762` — a reflected `RawTypeDef` (`not ported yet: reading the reflected form RawTypeDef`).

**These are not reader fixes.** Both are places where the port's *Syntax/Syntax-ADT
shape* differs from the prototype's, and reflection makes that shape observable to
macros. The macro domain model says reflection is **total** and the round trip is the
identity — so the ADT a macro can see is part of the language contract, not an
implementation detail. A macro written against the prototype's ADT can name a shape the
C# port cannot even represent.

## 1. Trait arity is one parameter / one argument

`Syntax.TraitDef`/`Binding.Trait` carry a single `Id Param`, `ImplDef`/`Impl` a single
`Arg`, and `TraitDecl.Operations` are closures over one binder — so a two-parameter
trait is unrepresentable, not merely unread. Probe: `DeclTrait(x, [x, x], …)`.

Decide first whether `fun` *has* multi-parameter traits, since `docs/wayfinder/tickets/`
has no decision on it and the prototype's support may be incidental (a list where one
element is used). If the language wants them, this is a trait-system rework — trait
resolution, bound sets, and the operations' binder shape all move — and it must be
settled on a ticket of its own before any code. If it does not, the prototype is
over-general and the port should say so loudly rather than grow a shape nothing uses.
**Get a ruling before forking this.**

## 2. `RawTypeDef`: the port has no such syntax node

The port has no `Syntax.TypeDef` node at all — stage 2 desugars `type` — so a reflected
`RawTypeDef` has nothing to produce. That is a genuine ADT difference, and the
prototype's node exists (`macro_eval.ml:762`).

This one needs a decision, not code: either the node comes back into the C# surface ADT
(as a shape macros may construct, even if the elaborator immediately desugars it), or
the ADT difference is recorded as deliberate, in which case a macro that reflects a
`TypeDef` must fail as a *macro* error naming the difference rather than as "not ported
yet". Which is right depends on whether the port intends the reflected syntax to be
identical to the prototype's — the porting conventions say the domain model is
definitive and the prototype is supporting material, so the argument belongs on the
domain model for macros (`docs/wayfinder/topics/core-tt-domain-model-macros.md`).

## Tests

Nothing here can be a shared conformance case while the shapes differ: a case would have
to reflect a `TypeDef` the C# surface cannot spell, so the OCaml runner would pass and
C# could not even express it — the "unported path" shape the audit exists to eliminate.
The honest interim state is an xUnit test asserting the refusal names the difference,
replaced by a shared case when the shapes agree.
