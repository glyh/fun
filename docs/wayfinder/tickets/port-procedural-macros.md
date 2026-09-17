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

## Progress (2026-09-17)

Merged from `port/macros` (`6456100`, `7a80ebd`, merge `18adb74`, `0282e17`,
`34306ea`, `489a24c`). All five steps: reflection over the prelude's `Syntax`
nominals, macro definitions and calls (expression and declaration position,
annotations, provisional macros), quoted syntax with holes, the runtime
(`IMacroRuntime` macro members, the budget as fuel, `expand_block`/`expand_decls`,
macros exported from units and delivered by `open`), and type-aware macros. 46
cases newly pass, each checked in both runners: 32 macro definition/call/quote
cases, 7 type-aware (core-198, 199, 202, 211, 212, 215, 216), and 7 new shared cases
(`imports/port-macro-open`, `-member`, `-private`; `macros/port-typed-unsolved-binder`,
`-argument-mismatch`, `-output-mismatch`, `-expected-mismatch`). The old per-file
`AddScope` traversals are gone in favour of `Syntax.Map`. C# 386/685; xUnit 168.

**Not done (ticket stays open):**
- **A macro body cannot see its unit's earlier top-level bindings.** The decided rule
  (M3: a macro is compiled "as of its definition") requires it; the prototype gets it
  by elaborating each top-level binding right after it expands
  (`Macro_driver.run_with`). The port expands a whole unit, then elaborates it, so
  `stage2.fun`'s `type_decls` fails (`unbound variable: List`, and its calls to
  earlier helpers). Needs unit expansion and elaboration interleaved per binding,
  through the macro runtime interface.
- **Operator macros** (`infix (~) (stx) { … }`, `core-190`), and stage 2's
  `pub infix (&&) …` declarations: not built.
- **Open (user):** `core-210`, `core-214` match on bare `RExpr(…)`, a constructor of
  the prelude's `Syntax.R`; under the bare-constructor ruling it is unbound (C#
  rejects them), and the prototype passes only through its by-name lookup.

**Stopgap (`ponytail:`):** a typed macro argument is elaborated twice (once to solve
the binders, again where the output places it); the prototype reuses the first.
