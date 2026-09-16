---
title: "Port: procedural macros"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: procedural macros

Wave 3 fork. Follow the porting conventions in
[port-core-tt-to-dotnet](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16).

## Scope, in order (commit each green)

1. **Reflection:** syntax objects as values of the prelude's `Syntax` nominals
   (`Expr`, `Decl`, `Pattern`, `TokenTree`, …, from `stdlib`) and back — the round
   trip is the identity, every field carried both ways (`CLAUDE.md` "Reflection and
   scope-addition: preserve ALL fields"); scope sets are opaque (`Scopes` atoms,
   M11/M12).
2. **Macro definitions and calls:** `macro name(params) { … }` in blocks and modules
   (`pub macro`), `: Expr(T)` / `: Expr(_)` / `: Decl` / `: List(Decl)` annotations,
   parameter kinds; calls in expression and declaration position; provisional
   macros during their own definition; operator macros (`CallMacro` roles).
3. **Quoted syntax:** `quote(…)` / `quote { … }` with holes, parsed at definition,
   hygienic (definition-site scopes pruned per Flatt; intro and use-site scopes).
4. **The runtime:** extend `IMacroRuntime` with elaborating and applying a macro
   under the evaluation budget (macro fuel *is* the budget); `expand_block` /
   `expand_decls` primitives; macros exported from units and delivered by `open`.
5. **Type-aware macros** (`: Expr(T)` with a promised type): deferred to the
   elaborator, which solves the macro's type binders and checks arguments and
   output (Klister-style interleaving, minus suspended expansions).

## Decided rules to read first

`docs/wayfinder/topics/core-tt-domain-model-macros.md`, `macro-interleaving-design.md`,
`docs/wayfinder/macro-system/` (design and papers), domain model I4c–I4e, glossary
phases section (Macro, Macro annotation, Provisional macro, Type-aware macro,
Syntax object, Reflection, Round trip, Template, Quoted syntax, Hole, Intro scope,
Use-site scope); closed tickets
[macro-annotation-constraints-mean-nothing](macro-annotation-constraints-mean-nothing.md),
[macro-type-binders-should-be-explicit](macro-type-binders-should-be-explicit.md),
[macros-have-no-quoted-syntax](macros-have-no-quoted-syntax.md),
[macro-bodies-implicitly-open-the-prelude](macro-bodies-implicitly-open-the-prelude.md),
[macro-fuel-is-the-evaluation-budget](macro-fuel-is-the-evaluation-budget.md),
[decl-macro-output-type](decl-macro-output-type.md),
[design-type-aware-macro-interleaving](design-type-aware-macro-interleaving.md),
[elaborator-macro-runtime-is-mutable](elaborator-macro-runtime-is-mutable.md).

## Target

The 54 cases blocked on "the `macro` form", and the prelude stage 2 source
(`dotnet/std/stage2.fun`) — its `type` macro is what the next ticket needs.
