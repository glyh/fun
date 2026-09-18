---
title: An impl head is a pattern over types
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee:
blocked_by:
  - trait-op-takes-innermost-impl.md
---

# An impl head is a pattern over types

Split off from [trait-op-takes-innermost-impl](trait-op-takes-innermost-impl.md)
(2026-09-18). That ticket lands generic impls whose head binds its free names. This
one is the rest of the idea: the head is a **pattern**, matched by the mechanism the
language already has, rather than a trait application with variables in it.

## The idea

```
pattern Container(A) = Option(A) | List(A);

impl Size(Option(A))    = module { size = fn(o) { 1 } };   -- lands with the other ticket
impl Size(Container(A)) = module { size = fn(c) { 2 } };   -- a synonym as a head
impl Size(_)            = module { size = fn(x) { 0 } };   -- blanket, least precise
```

Types are values and `Type` is open, so a pattern over types is just a pattern.
Resolution becomes: match the use's argument types against each head, then order the
matches by precision (`traits.md` rule 2).

## Why it is plausible rather than speculative

Patterns are already first class at compile time. `std/stage1.fun` ships the
`Syntax.Pattern` nominal with `RawPatWild`/`RawPatBind`/`RawPatCon`/`RawPatOr`, the
builders `pat_wild`/`pat_var`/`pat_con`/`pat_atom`/`pat_prod`/`pat_or`, and the
destructuring synonyms `PatWild`/`PatBind`/`PatCon`/`PatAtom`/`PatProd`/`PatOr`.
`Captured` has `CapPattern(Pattern)` and `HoleKind` has `HolePattern`, so a macro can
take a pattern as an argument; `Decl` has both `DeclPatternSyn` and `DeclImpl`, and a
`Decl`-position macro returns `List(Decl)`. A `derive`-style macro that computes
patterns and emits impls is therefore writable today, with no language change.

## What it costs

**One ADT change, touched in six places.**

```
DeclImpl(Option(Id), Path, List(Expr),    List(Field), Bool)   -- today
DeclImpl(Option(Id), Path, List(Pattern), List(Field), Bool)   -- proposed
```

Macros reflect over `Syntax.Decl`, so per `CLAUDE.md` ("Reflection and scope-addition:
preserve ALL fields") this ripples through reflection both ways (`macro_eval.ml`),
`expand.ml`'s `go_kind`/`go_struct_binding` and `map_binders`, `enforest_template.ml`'s
rules, `wrap`/`unwrap`, `syntax_nominals`, and their C# equivalents.

**Decidability is preserved.** Heads stay first-order patterns, so matching
terminates; the computation that *produces* a head is a macro, and macros already run
under `Metas.Budget`. Arbitrary compile-time impl selection does not follow.

**Readability is not.** If impls can be computed, the candidate set is no longer
visible in the source: "which impl did this pick" becomes a question answered by
running the macro. A diagnostics cost, not a soundness one, and diagnostics are
deferred.

## Open questions

1. **Or-patterns: one candidate or two?** `Container(A)` expands to
   `Option(A) | List(A)`. Per-branch candidacy is the sane reading — each branch is
   ordered against the other heads on its own — but it is not written down.
2. **Blanket `_`.** Least precise, always last; everything is an instance of it. Falls
   out of rule 2, but confirm it is wanted at all rather than an accident of allowing
   patterns.
3. **Matching must suspend on an unknown, not fail.** Rule 4 says unknown argument
   types make the choice wait. `Size.size(x)` with `x`'s type still a meta must park,
   not fall through to `impl Size(_)`. `Core_match_compile` has no stuck case, so this
   is the same shape as `match`, not the same code.
4. **Scope.** A pattern synonym is a binding, so two modules' `Container` may differ
   and an impl head means whichever was in scope where the impl was written. Believed
   to need no new rule (sets-of-scopes handles it); confirm.

## Not blocking

Nothing needs a synonym head yet. Drive this from a real call site — most likely
stage 2's library code once generic impls exist — rather than deciding it in the
abstract.
