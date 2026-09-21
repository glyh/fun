---
title: "Port: a reflected TypeDef (a trait takes one parameter — ruled a divergence)"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: a reflected `TypeDef`

The one item left from [the latent gaps](port-latent-form-gaps.md) fork's G3, after
[the reflected Syntax ADT](port-reflected-adt-differs.md) was ruled on. Two things were
bundled there; the ruling split them.

## Ruled (integrator, 2026-09-20, on evidence)

**A trait keeps its single parameter; multi-parameter traits are a divergence, not a
port.** Checked rather than argued: every trait in the language — `Eq(A)` in
`dotnet/std/stage2.fun` and in `lib/semantic/typecheck/elab_prelude.ml`, plus `Show(A)`,
`Size(A)`, `Same(A)` across the conformance cases — takes exactly one parameter. The
prototype's list is a generalisation with no user. So `Reflection.cs:718`/`:922` and
`:726`/`:928` should raise a **named macro error** saying the port's traits carry one
parameter, not `not ported yet`, and the difference is recorded as deliberate. If the
language ever wants multi-parameter traits that is a language question with its own
ticket — it is not a reflection fix.

## To port: the `TypeDef` shape

`Reflection.cs:762` — a reflected `RawTypeDef`. Probe: prototype `1`, port refuses. The
port has no `Syntax.TypeDef` node because stage 2 desugars `type`, so the reader has
nothing to produce — but a macro written against the prototype's ADT can name that shape,
and the SAME code cannot be *written* in C# at all. Reflection is total and the round
trip the identity, so this is a hole in a total function, not a nicety.

Fix: restore `TypeDef` as a shape a macro may construct and reflect, with the elaborator
desugaring it immediately so nothing downstream changes. Check what the prototype's
`macro_eval.ml:762` reader accepts and what its writer produces, so the round trip is
the identity in C# too. Note `Syntax.TypeDef` existed before the port (`delete-surface-ir`
removed `Surface.t`, not this) — look for the shape the OCaml reader expects rather than
inventing a new one.

## Tests

`RawTypeDef` is macro-constructible, so this *can* be a shared case: a macro that
constructs a `TypeDef` and a macro that reflects one, both `expect` a value. Verify in
both runners before committing — the prototype reads it (`1`), so if a case fails in
OCaml the case or the reading is wrong, not the prototype. Add the xUnit round-trip
assertion too (construct → reflect → construct is the identity), mirroring
`AParameterWithTraitBoundsRoundTrips`.
