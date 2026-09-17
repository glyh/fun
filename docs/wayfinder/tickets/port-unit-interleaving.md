---
title: "Port: interleave a unit's expansion and elaboration; operator macros"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
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
