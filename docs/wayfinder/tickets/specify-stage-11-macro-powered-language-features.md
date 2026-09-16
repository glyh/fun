---
title: Stage 11 macro-powered language features spec
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee: glyh
blocked_by:
  - design-type-aware-macro-interleaving.md
---

# Stage 11 macro-powered language features spec

## Question

Specify what Stage 11 should actually include, given that no spec for it
currently exists.

## Context

- Macro Stages 0–10 are complete.
- Likely candidates for Stage 11 include derived helpers (e.g. `#[derive]`
  equivalents), DSL blocks, syntax scaffolding, and library-level feature
  experiments that exercise the macro system.
- Should avoid building new compiler machinery unless strictly necessary for
  the macro feature being tested.

## Direction (decided) — ticket stays open, more increments planned

**Stage 11's flagship is: demote language constructs hardwired in the compiler
core down into library-level definitions** — proving the type theory carries its
own surface syntax instead of growing more built-in machinery. This is the
"macro-powered language features" theme made concrete. The ticket remains open as
the umbrella for successive demotion increments.

## Progress

**Increment 1 — `Bool` + `if` (implemented, green: 778 tests).** `Bool` is now a
prelude nominal ADT (`type Bool = False | True`), primitives return `I64` and the
prelude wraps them, and `if` is desugared to `match` (the dedicated `Core.If` node
and `FIf` frame were removed, sound because `FMatch` already subsumes them). Two
latent lib regressions surfaced by the change were fixed (effects in match/if
branch bodies; constructor patterns in tuples). Design + detail:
[Bool and `if` as library features](../topics/bool-and-if-as-library.md).

## Candidate use cases

A curated shortlist of macro use cases that exploit `fun`-specific capabilities
(types-as-values, type-providing macros, type-case + record reflection, traits as
dictionaries, effects) — the idea store for future increments:
[macro use-case shortlist](../topics/macro-use-case-shortlist.md).

## Further increments (graduated into their own tickets)

- [Add short-circuit && / || operators](add-short-circuit-and-or-operators.md)
  — feasible now via the builtin operator table.
- [Explicit prelude open for operator demotion](explicit-prelude-open-operator-demotion.md)
  — needed before `+`/`==`/`<` can move out of `operator_env.ml`; blocked on
  [Unify operators into the scope-aware binding table](unify-operators-into-scope-aware-binding-table.md).
- [Reflect Match in the Expr macro ADT](reflect-match-in-expr-macro-adt.md)
  — required for a *true* prelude-macro `if` (macros can't construct `Match` today).

## Progress — increment 2 (2026-09-16): the keyword surface

Survey of what is still compiler-built found the library demotions already done:
`if`/`else` (prelude `pub syntax if`), `&&`/`||`, every arithmetic and comparison
operator, prefix `not`, and `type` (a stage-2 std macro since the staged prelude).
`Match` and `Branch` are reflected in the `Expr` ADT, so
[reflect-match-in-expr-macro-adt](reflect-match-in-expr-macro-adt.md)'s blocker is
gone and `if` already is a prelude form.

**Landed:** five keyword tokens deleted — `then`, `with`, `end`, `else`, `Unit`.
Nothing matched them: a syntax form's rule literals compare by spelling
(`Enforest_template.same_literal_token`), so the prelude's `if` form matches
`else` as a plain token, and `Unit` had an identical `Ident` path in expression
and pattern position. They are ordinary identifiers now
(`test/conformance/cases/values/freed-keywords.fun`).

## What remains

- **`ref` / `deref` as primitives** — needs a decision. `RefNew` mints a *fresh*
  heap meta per occurrence (`elab_infer.ml`, "a new reference starts its own
  heap"). As a primitive its type would be
  `[h : Type, A : Type] -> A ->{Mutate(h)} Ref(h, A)`, where `h` is solved by
  unification: normally a fresh meta (same behaviour), but an expected type could
  force an existing heap, widening what is discharged. `RefNew` also carries E11's
  generative module stamp, so the core node stays either way.
- Everything else keyword-driven (`match`, `fn`, `struct`, `module`, `sig`,
  `enum`, `trait`, `impl`, `effect`, `macro`, `pattern`, `import`, `open`,
  `export`, `perform`, `resume`, `method`, `rec`, `pub`, `self`, `Self`) is a core
  structural form producing a primitive node — keywords by the rule decided
  2026-09-16. No `while` exists to demote.
- Not this ticket, noticed: `enforest_pat.ml` picks type patterns (`I64`, `Unit`,
  `Char`, `String`, `Absurd`) by spelling — an M12 survivor.
