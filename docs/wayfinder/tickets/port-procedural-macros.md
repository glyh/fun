---
title: "Port: procedural macros"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-25
resolution: Closed 2026-09-25 after a read-only re-audit verified every scope bullet landed in dotnet/src - by the mechanism, not by the prose. The wave itself landed 2026-09-17; the ticket stayed open by bookkeeping, like port-generative-former-identity-residue.
assignee:
blocked_by:
---

# Port: procedural macros

> ## Resolution (2026-09-25) — every scope bullet verified landed
>
> Verified by a read-only audit against `dotnet/src`, and re-checked by the integrator:
>
> 1. **Reflection** — `Reflection.cs`, exercised by the port's own xUnit round-trip tests.
> 2. **Definitions and calls** — `Expander.Macros.cs`, including `ExpandOperatorUse` for
>    operator macros (`stage2.fun`'s `pub infix (+)` / `(&&)` run), in blocks and modules.
> 3. **Quoted syntax** — `Enforest.Roles.cs`'s block reading, parsed at definition.
> 4. **The runtime, including this ticket's own "not done" headline — the interleaving** —
>    `Fun.Expand/MacroRuntime.cs:57` `void Advance(Binding expanded)`, called per expanded
>    binding by `Expander.cs:244`, with `UnitRuntime.Advance` → `Elaborator.AdvanceUnit`
>    (`Elaborator.cs:466`) and the loader's own implementation. Expansion and elaboration
>    interleave one top-level binding at a time.
> 5. **Type-aware macros** — `ApplyTypedMacro` (`Elaborator.cs:218,329`).
>
> One refusal remains on this path — `Expander.Macros.cs:306`, a *typed operator* macro — and it
> is ruled [post-port work](port-typed-operator-macro.md): the prototype hangs on it too, so it
> is parity by absence, not a gap.

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
- **Decided (user, 2026-09-17):** `core-210` and `core-214` matched on a bare
  `RExpr(…)`, unbound under the bare-constructor ruling; both now write
  `Syntax.RExpr(…)`, keeping what they test (type-aware macros). Both pass in the
  prototype; in C# `core-210` passes and `core-214` waits on the prelude's `+`.

**Stopgap (`ponytail:`):** a typed macro argument is elaborated twice (once to solve
the binders, again where the output places it); the prototype reuses the first.
