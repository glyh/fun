---
title: An impl head is a pattern over types
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee:
blocked_by:
  # trait-op-takes-innermost-impl.md closed 2026-09-27, and
  # type-case-patterns-cannot-express-impl-heads.md closed the same day as
  # superseded; both edges were replaced by this one.
  - pattern-binders-are-lowercase-and-references-are-pinned.md
---

# An impl head is a pattern over types

Split off from [trait-op-takes-innermost-impl](trait-op-takes-innermost-impl.md)
(2026-09-18). That ticket lands generic impls whose head binds its free names. This
one is the rest of the idea: the head is a **pattern**, matched by the mechanism the
language already has, rather than a trait application with variables in it.

## Re-measured 2026-09-27 — the premise, corrected

Probed on the Debug runner while grilling this ticket (everything below is
measured, not inferred). **Its "one ADT change, touched in six places" estimate is
wrong in both directions, and its "Not blocking: nothing needs a synonym head yet"
is the accurate part.**

Already available today, no ADT change: `impl Trait(_)` is a blanket head at any
depth (`impl Size(Option(_))` served `Option(I64)` *and* `Option(Bool)`, and loses
to a specific `impl Size(I64)` — by precision, not declaration order); nested and
aliased heads (`Option(Option(A))`, `Seq = fn(A : Type) { Option(A) }` with
`impl Size(Seq(A))`); generic heads (`impl Size(Option(A))`, cased
`trait-generic-impl`).

Premise corrections:

- **`Arg` is one `Syntax`, not `List(Expr)`.** `Binding.Impl(Id? Name, Syntax
  TraitPath, Syntax Arg, …)` (`Syntax.Traits.cs:36`); reflection already reflects it
  as a one-element list (`Reflection.cs:454`) and unwraps exactly one (`:978`). The
  proposed `List(Expr) → List(Pattern)` is a one-slot change.
- **"`Core_match_compile` has no stuck case" is prototype prose.** The port has
  `StuckNeutral` (`Nbe.StuckMatch.cs`: *"a match waits as the unknown value's last
  frame"*). One of the ticket's two cost items is gone.
- **A head has no implicit width subtyping to lose.** `impl Size(struct { a : I64 })`
  does not match a use at `struct { a = 1; b = True }` — nor even at
  `struct { a = 1 }`: `missing implementation`. (The `Matches` doc comment's "width
  subtyping" concerns struct *modules*, not record types.) Adjacent unknown worth
  probing before designing on it: `x : struct { a : I64 } = struct { a = 1 }` is
  refused with `structs with different members`, so the failure may be about struct
  *member kinds* rather than width.
- **A name in a head, and what it means, is settled elsewhere** — [A pattern binder
  is lowercase; naming an existing term takes
  `^`](pattern-binders-are-lowercase-and-references-are-pinned.md). So this ticket
  does not need its ADT change to resolve a head.

**What is left is two things:**

1. **An or-pattern in a head — decided 2026-09-27: one declaration, `match`'s
   or-pattern rules.** `impl Size(Option(a) | List(a))` (today `expression has
   trailing terms`) is **one impl with N alternatives**: every alternative must
   bind the same names, each alternative is a resolution candidate, and whichever
   matches supplies the body — evaluated once, in one binder context. Mechanism:
   `Binding.Impl`'s single `Arg` becomes a list of alternative type expressions,
   which the reflected `DeclImpl` already carries (`Reflection.cs:454` reflects
   `List([i.Arg], ReflectExpr)`, `:978` unwraps exactly one), so the reflection
   side gets simpler rather than bigger. **Rejected: an abbreviation for N
   declarations.** It needs no syntax at all, but it elaborates the same body
   text once per alternative under *different* binders — the head-shaped version
   of the site-adapts-behind-your-back mechanism this project deleted from the
   macro system (`macro-owns-its-output`'s ruling that `Syntax.publish` goes).
2. **A width-tolerant head** — `impl Size(struct { a : I64; _ })` is today
   `unsupported module item: _`. Now cheap: `StructType` is already a
   `NeedsDirectMatch` pattern (`Core.Patterns.cs:61`) and any such pattern swaps the
   whole match to `Sequential` (`Elaborator.Match.cs:71`), so a `struct { …; _ }`
   head rides machinery that exists. It is also the only form that creates a new
   *kind* of ambiguity — `struct { a : I64; _ }` and `struct { b : Bool; _ }` both
   match `struct { a = 1; b = True }` — so it needs rule 3's answer for width, not
   just the syntax.

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
