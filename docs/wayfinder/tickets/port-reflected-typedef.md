---
title: "The reflected TypeDef node is deliberately absent — `type` is a macro"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-20
resolution: Ruled (user, 2026-09-20) - do not port it. `type` is a macro in both implementations, so a dedicated node for it in the macro-visible ADT is meaningless; the port's absence is the newer shape and the prototype's RawTypeDef is a leftover.
assignee:
blocked_by:
---

# The reflected `TypeDef` node is deliberately absent

**Ruled (user, 2026-09-20): do not port it.** This reverses the integrator's earlier
call on this ticket, and the reason is better than the one it replaced.

## Why

`type` is a **macro**, in both implementations:

- `lib/semantic/typecheck/elab_prelude.ml:292` —
  `pub syntax type : Decl { type $(r : List(TokenTree)) => { type_decls($r) } }`, over the
  public `type_decls` macro.
- `dotnet/std/stage2.fun` — the same, `type_name`/`type_params`/`type_member`/`type_opens`/
  `type_exports` building `rec … and …` enums plus their `export` and `open`.

And the surface nodes it replaced are already gone: [ADTs are declared by let
bindings](adts-as-let-bindings.md) (closed) lists `Syntax.TypeBinding`, `DeclType`, the
`TypeDeclaration` role and `parse_type_binding` as **deleted**. So a `TypeDef` node in
the *macro-visible* ADT has nothing that produces it, and giving macros a node for a
construct that is itself a macro is a contradiction — the macro's own output is the
`rec … and …` form, and that is what a macro should see.

The port is therefore **ahead** here, not behind: its ADT has no such node because
stage 2 desugars `type` before anything can observe it. The prototype's `RawTypeDef`
(`lib/expand/macro_eval.ml:762`) is a leftover of the pre-macro `type`; a hand-written
macro can still build one there, which is exactly the kind of shape no source can
produce.

## Action

`dotnet/src/Fun.Compiler/Reflection.cs:762`'s refusal becomes a **named macro error**
(not `not ported yet: reading the reflected form RawTypeDef`), so a macro that asks for
this shape is told the ADT is narrower on purpose. Record it as a deliberate divergence
in the reflected syntax:

- the shared-suite angle cannot exist (a case would have to reflect a shape the C#
  surface cannot spell), so nothing is listed in `prototype-divergences.txt`;
- an xUnit test asserting the refusal *names the difference* is the honest interim, as
  `port-reflected-adt-differs` said.

If the prototype is ever corrected, its `RawTypeDef` should be deleted rather than read —
but the prototype is not maintained after the port, so this is a note, not work.
