---
title: "Port: interleave a unit's expansion and elaboration; operator macros"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
assignee:
blocked_by:
---

# Port: interleave a unit's expansion and elaboration; operator macros

Wave 3 fork. Follow the porting conventions in
[port-core-tt-to-dotnet](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16).

## Scope

1. **Interleaving.** A unit (module items, and a block's statements) is expanded and
   elaborated one top-level binding at a time: a binding is expanded, then
   elaborated, before the next is read — so a macro defined later compiles *as of
   its definition* (M3), seeing the unit's earlier bindings (values, types,
   helpers). The prototype does this in `Macro_driver.run_with`; the port today
   expands a whole unit, then elaborates it. Drive it through `IMacroRuntime`
   (Fun.Expand still never references Fun.Compiler): the design is ordered
   interleaving borrowed from Klister, minus suspended expansions.
2. **Operator macros:** `infix (~) (stx) { … }` / `prefix` operator declarations
   whose body is a procedural macro (`CallMacro` roles), and their uses
   (`core-190`); stage 2's `pub infix (&&) …` declarations.
3. **Stage 2 compiles:** `dotnet/std/stage2.fun`'s `type_decls` macro and the rest of
   stage 2 must *compile* (expand and elaborate as a unit) — binding it as the prelude
   is the next ticket. Do not edit `stage2.fun`; if it still fails, report exactly
   why.
4. Retire the typed-argument double elaboration (`ponytail:` from the macros fork) if
   interleaving makes the first result reusable.

## Decided rules to read first

`docs/wayfinder/topics/macro-interleaving-design.md`,
`docs/wayfinder/macro-system/TYPE_AWARE_INTERLEAVING.md`,
[design-type-aware-macro-interleaving](design-type-aware-macro-interleaving.md),
domain model I4c–I4e and I5, glossary **Provisional macro**, **Type-aware macro**,
**Expansion position**; the macros merge record in
[port-procedural-macros](port-procedural-macros.md).

## Resolution (2026-09-17)

Merged from `port/interleaving` (`74fa314`, `4d303f1`, `b933e0b`, `e255f36`, head
`7905031`). A match on an unknown value waits as a neutral `FMatch` frame;
`V = import "v"; open V` is an open of unit `v` (`unit:v`); `Expander.ExpandUnit`
hands each top-level binding to `IMacroRuntime.Advance`, which a per-unit
`UnitRuntime` elaborates (`Elaborator.ElaborateBinding` shared), so a macro compiles
as of its definition; `pub (<) = …` binders parse; procedural operator macros
(`infix (~) (stx) { … }`) with an `OperatorUse` form that reflects as
`RawOperatorUse` and round-trips; a macro binder counts as a role for role mixing
(M7). **`stage2.fun` compiles** as a unit importing `std`
(`InterleavingTests.Stage2CompilesAsAUnit`); binding it as the prelude is next. New
shared cases: `imports/unit-macro-sees-earlier-binding` (5),
`imports/unit-macro-not-later-binding` (error), `values/stuck-match-in-type` (5) —
agreeing with the prototype — and `imports/unit-handle-open-form-member` (3), a
prototype defect ([unit-handle-open-not-a-unit-open](unit-handle-open-not-a-unit-open.md)).
`macros/core-190` passes. C# 393/689; xUnit 172.

**Not done:** retiring the double elaboration of typed macro arguments — typed
arguments are elaborated at the call inside the elaborator, not at a unit's top
level, so interleaving does not make the first result reusable (`ponytail:` stays).
**Scope:** only a unit's top level advances, not a block's statements: a macro body
sees no local binder (glossary), and the prototype advances only units.
