---
title: Reflect Match in the Expr macro ADT
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee:
blocked_by:
---

# Reflect Match in the Expr macro ADT

> ## Re-measured 2026-09-26 — the work looks landed, but nothing tests it
>
> The stale `blocked_by:` edge (`mutually-recursive-nominal-types.md`, closed) is dropped. The
> design below was implemented against the port and **not** the `macro_eval.ml` this ticket was
> written for:
>
> - `RawMatch` and `Branch` are in the reflected `Expr` ADT — `std/stage1.fun:32`, and the
>   `Branch` sum is already the forward-compatible `ValueBranch | EffectBranch` shape decision 2
>   asked for (it went further than "value branches only").
> - Both directions round-trip: reflect at `src/Fun.Compiler/Reflection.cs:268` (+ `BranchVal`
>   at `:353`, both branch kinds) and read at `:739` (+ `ReadBranch` at `:868`).
> - The patterns it needed are reflected too (`RawPatWild/Bind/Con/Atom/Prod/Or/Record/StructType/Type`).
> - The follow-on it was meant to unblock is already true: `if` is a prelude form, not an
>   enforester desugar (see [Stage 11](specify-stage-11-macro-powered-language-features.md)).
>
> **Unowned gap:** `grep -r 'RawMatch' test/conformance/cases` returns nothing, and no xUnit case
> names it either — so *whether a macro can actually construct and destructure a `match`* is
> **unprobed**. One probe settles it: a prelude-free program whose macro builds
> `RawMatch(scrutinee, [value_branch(pat, body)])` and one whose macro destructures a `match`
> passed to it, each as a `test/conformance/cases/macros/` case. If they pass, this ticket closes
> with those two cases and the ADT decision moves to the map's Decisions-so-far; if either fails,
> the failure names what is left.

## Question

Extend the reflected `Syntax.Expr` ADT (and the `macro_eval.ml` marshalling) with
a `Match` constructor (and the pattern reflection it needs), so macros can
*construct* and *destructure* `match` expressions — enabling a **true
prelude-macro `if`** and other library-defined control forms.

## Context

- Graduated from [Stage 11 macro-powered language features spec](specify-stage-11-macro-powered-language-features.md).
- Today macros can only build the reflected `Expr` constructors
  `RawVar/RawAtom/RawAp/RawLam/RawLet` (`elab_prelude.ml`); any other node
  (`Match`, `If`, `Struct`, …) falls through to an opaque `VStx (StxExpr …)`
  passthrough (`macro_eval.ml`). That is why `if` had to be desugared in the
  enforester rather than defined as a prelude macro — see
  [Bool and `if` as library features](../topics/bool-and-if-as-library.md).
- The `Pattern` ADT is already partly reflected (`RawPatWild/Bind/Con/Atom/Prod/Or`),
  so `Match` reflection can reuse it; the branch structure (value vs effect
  branches, `resume`) needs a reflected form too.
- Once available, `if` could move from the enforester into prelude source as a
  macro/template, further shrinking the compiler-built-in surface.

## Design decisions (from grilling)

1. **Branch scope: value branches only.** Reflect value branches (`pattern -> body`)
   now; defer effect-handler branches — they'd also require reflecting `resume` and
   effect paths, and none of the driving use cases (`derive`, `if`, `matches?`,
   pattern DSLs) synthesize handlers.
2. **Representation: a dedicated `Branch` ADT** — `RawValueBranch(Pattern, Expr)`
   with smart ctor `value_branch` and pattern synonym `ValueBranch`; the node is
   `RawMatch(Option(Span), Expr, List(Branch))`. Chosen over a bare
   `List(Pattern * Expr)` tuple for named clarity and forward-compatibility to a
   `ValueBranch | EffectBranch` sum. **Blocked by a language gap:** `Expr` and
   `Branch` are mutually recursive (`RawMatch` carries `List(Branch)`; `Branch`
   carries an `Expr` body), and `fun` has no mutually-recursive nominal types today
   — see **[Mutually-recursive nominal type declarations](mutually-recursive-nominal-types.md)**,
   which this ticket is now blocked on. (Structural fallbacks `List(Pattern * Expr)`
   or `List(struct pattern: Pattern; body: Expr end)` both verified to work and would
   unblock without the language change, but we chose the clean ADT.)
3. **Directionality: construct *and* destructure.** Both `unwind_stx`
   (`RawMatch` -> `Syntax.Match`, for macros that build matches) and `wrap_stx`
   (`Syntax.Match` -> `RawMatch`, for macros that rewrite an input match), plus the
   `Match`/`ValueBranch` pattern synonyms.
4. **Input-side fallback: reflect-what-maps.** Reflect any all-value-branch match as
   `RawMatch`, letting individual non-reflected patterns (record/struct/type) sit
   inside as opaque pattern values — they already round-trip via `StxPattern`
   (`macro_eval.ml:433` wrap, `:396` unwrap, verified). Fall back to a whole-match
   opaque `VStx (StxExpr …)` **only** when a branch is genuinely unrepresentable,
   i.e. an effect branch. This is *less* special-casing than all-or-nothing (it just
   reuses the existing `wrap_stx_pat`).

## Implementation sketch (once unblocked)

- Prelude `Syntax` module (`elab_prelude.ml`): add the mutually-recursive
  `Expr`/`Branch` group with `RawMatch` + `RawValueBranch`, plus `value_branch` /
  `match_` (note `match` is a keyword — pick a non-keyword ctor name) smart ctors and
  `Match` / `ValueBranch` pattern synonyms.
- `macro_eval.ml`: `wrap_stx`/`wrap_stx_sub` gain a `Syntax.Match` case (reflect-what-maps
  per decision 4); `unwind_stx` gains the `RawMatch` -> `Syntax.Match` case; reuse
  `wrap_stx_pat`/`unwrap_stx_pat` for branch patterns.
- Thread the new `Branch` nominal through `syntax_nominals` and every construction
  site (the CLAUDE.md checklist).
- Follow-on it unblocks: a true prelude-macro `if`, `matches?`, pattern DSLs, and the
  `derive` flagship (with Decl reflection widened separately).

## Resolution

_Unresolved (blocked on mutually-recursive nominal types)._
