---
title: Stage 11 macro-powered language features spec
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee: glyh
blocked_by:
---

# Stage 11 macro-powered language features spec

> **Unblocked 2026-09-26** — `design-type-aware-macro-interleaving.md` closed. Re-framed the same
> day: the ticket was written while two implementations existed, and the counts and file paths in
> it are the OCaml prototype's. The prototype is gone (2026-09-25) and `src/` is the
> implementation, so progress is measured against the port from here on. The direction
> (demote compiler-built-in constructs into the library) is unaffected and still decided.

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

**Increment 1 — `Bool` + `if` (implemented).** "778 tests" was the prototype's `dune test`
count; the port's equivalent measurement on 2026-09-26 is `conformance: 773 cases, 0 failed` and
xUnit `185/185`, and the same behaviour is in it. `Bool` is a
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
  — the ticket and [Unify operators into the scope-aware binding table](unify-operators-into-scope-aware-binding-table.md),
  which blocked it, are both closed. **Re-measured 2026-09-26 on the port:** the comparison and
  equality operators are already prelude definitions over primitives
  (`pub (<) = fn(x, y) { … }` and the `Eq` impls, `std/stage2.fun:13-25`), while the arithmetic
  five are still primitives (`+` at `src/Fun.Compiler/Primitives.cs:44`). So the demotion is
  half-done and no longer blocked; what is left is the `+`/`-`/`*`/`/`/`%` half, whose cost is
  whatever the fixity declaration already handles (they are `pub infix` in `std/stage2.fun:31-35`).
  Whether that half is wanted is this ticket's call, not a blocker.
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
(`Enforest_template.same_literal_token`, now `SameLiteral` in
`src/Fun.Expand/Enforest.Roles.cs`), so the prelude's `if` form matches
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
  generative module stamp, so the core node stays either way. (Prototype path `elab_infer.ml`;
  the port's refs live in `src/Fun.Compiler/Elaborator.Refs.cs`, whose note is the same: "each
  `Ref(A)` gets a fresh one".)
- Everything else keyword-driven (`match`, `fn`, `struct`, `module`, `sig`,
  `enum`, `trait`, `impl`, `effect`, `macro`, `pattern`, `import`, `open`,
  `export`, `perform`, `resume`, `method`, `rec`, `pub`, `self`, `Self`) is a core
  structural form producing a primitive node — keywords by the rule decided
  2026-09-16. No `while` exists to demote.
- Not this ticket, noticed: the prototype's `enforest_pat.ml` picked type patterns (`I64`,
  `Unit`, `Char`, `String`, `Absurd`) by spelling — an M12 survivor. **Re-measured 2026-09-26:**
  the port resolves them structurally, as `Pattern.AtomType(AtomTy Ty)`
  (`src/Fun.Kernel/Syntax.Patterns.cs:27`, elaborated at `src/Fun.Compiler/Elaborator.Match.cs:184`,
  `:416`) — the spelling lookup did not port, so there is nothing to demote here.

## Grilled (2026-09-16): `ref` / `deref` stay compiler nodes

Not demoted to the library. Each `ref(…)` must mint a **fresh** heap by
construction; as a primitive `ref : [h, A] -> A ->{Mutate(h)} Ref(h, A)` the heap
would be an ordinary meta that unification could merge with an existing one,
breaking the discharge rule that local mutation is private. `RefNew` also carries
E11's generative stamp. Considered and rejected: moving them plus a
"these heaps are distinct" constraint — more machinery than the node it removes.
