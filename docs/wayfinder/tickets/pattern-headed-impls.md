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
   **Held, not built (user, 2026-09-27)** — no call site needs it, and unlike the
   pin it adds no capability: a macro can emit `Option(a)` and `List(a)` as two
   declarations with the same body. Build it when a call site appears, which is
   the instruction this ticket already carried ("drive this from a real call
   site").
2. **A width-tolerant head — decided 2026-09-27: build it, as a pattern-valued head.**
   `impl Size(struct { a : I64; _ })` is today `unsupported module item: _`. The
   ruling: a head's argument may be a *pattern*, and a struct pattern with a rest is
   the first form that needs it (the pin already put pattern syntax inside a head,
   stage 1 of [the binder rule](pattern-binders-are-lowercase-and-references-are-pinned.md);
   this is the same door, opened wider).

   **Correcting the framing this ticket carried while it was being probed.** The
   earlier notes here called the cost "partial record types" and the effect "width
   subsumption the type system denies". Both are wrong. A head is not a type
   annotation — it is a condition on a type, which is this ticket's own thesis — so
   `impl Size(struct { a : I64; _ })` adds no type to the language: nothing changes
   for `fn(x : struct { a : I64; _ })`, which stays an error, and the dictionary it
   supplies is still an ordinary `Size(R)` verdict for each `R` the pattern matches.
   Nothing about typing is loosened and no soundness question is raised. What the
   form *does* buy is the thing this ticket was always about: **a head that
   decomposes a record type**, so `impl Size(struct { a : p; _ }) = …` can bind the
   field's *type* `p` and use it in the body — structural reflection at resolution
   time, which is `derive`-by-structure for the first time.

   Costs, all real, none of them "cheap":
   - **`ResolveEvidence` gains a pattern test.** Today a candidate is a list of type
     *values* compared by `Matches` (unification). A pattern-valued head is tested
     against the use's argument types instead — one new path, not a new matcher:
     `CorePattern.StructType` already carries `Partial` and already matches a
     `VStruct` (cased in `type-case-struct-field-type.fun`).
   - **Rule 2 needs pattern subsumption.** `Instance(p, q)` currently asks whether
     one *value* is an instance of another; with a partial head it must ask it of
     two *patterns*. That relation is the one the new unreachable-arm check needs
     anyway — build it once, use it twice.
   - **Rule 3's ambiguity becomes reachable in a new way**, which is the *existing*
     answer, not a new rule: `struct { a : I64; _ }` and `struct { b : Bool; _ }` are
     incomparable, so a use matching both is `ambiguous implementation`.
   - **Ordering is by precision, not by width**, and the two are not the same
     question: `struct { a : I64; b : Bool; _ }` is more precise than
     `struct { a : I64; _ }` (an instance of it), so it wins wherever both match.

   **Decided 2026-09-27 (user): a head pattern's field-type binder IS the impl's own
   variable, usable in the body.** `impl Size(struct { a : p; _ }) = module { size = fn(x) {
   Size.size(x.a) } }` works, and the body's demand on `p` becomes a hidden dictionary
   argument through machinery that already exists (`cb52e96`). Two *cased* behaviours compose
   to give this, which is why the alternative is the worse one: a head's own variables already
   bind and become hidden arguments (`trait-generic-impl-two-bounds`: `impl Size(Tuple(2, a,
   b))` with `Size.size((1, 'c'))` → 3), and a struct pattern already binds a field's type for
   the body to use (`type-case-struct-field-type`). Refusing field-type binders would add the
   first pattern position in the language where a binder is not allowed.

   **Decided 2026-09-27 (user): the rest form is admitted at any depth.** Measured first, so
   the choice is against facts, not taste: a nested structural head with *exact* fields
   already works (`impl Size(Option(struct { a : I64 }))` matched `Some(P{a = 1})` and did
   not match `Some(R{a = 1; b = True})`, which has an extra field), and a nested pin is cased
   (`impl Size(Option(^z))` in `pattern-pin-impl-head.fun`). So `_` at depth is the
   consistent rule, not an exception: `impl Size(Option(struct { a : p; _ }))` means *a
   container of any record having field `a`, delegating to that field's evidence*. The
   top-level-only alternative would need a positional check for the one form in a head that
   is not admitted at depth.

   **Decided 2026-09-27 (user): matching compares a record's *fields*; a method never hides
   it.** Every cell measured against `impl Size(struct { a : I64 })` and a use
   `U{a = 1}` where `U = struct { a : I64; <one extra member> }`: a **private** binding member
   is invisible (matched → 1); a **public** binding is not (→ 0); a **public method** is not
   (→ 0, `mm5`); a `pub impl` member is invisible (→ 1). So today a public method silently
   removes a record type from the range of every head naming its fields — which would have
   made the width form useless on exactly the records it is wanted for (a record with
   behaviour).
   The rule: **matching and coverage compare field sets and ignore everything else**, while
   **types stay distinct** — equality still compares all shown members, so
   `x : P = U{a = 1}` remains `structs with different members` (measured, `ex1`), and an
   extra *field* still needs the opt-in `struct { a : p; _ }` (measured today: an extra field
   with no `_` does not match, `ex2`). Access goes by the type as written. That is the same
   "resolution more permissive than typing" licence the width form already takes.

   An earlier note here said *"a record type IS its fields"* and read that as the two being
   the **same type**. That was wrong and is retracted: the precedents (PureScript, Elm,
   TypeScript) do **directed subtyping** — a wider record is usable where a narrower type is
   expected, one direction, and member access uses the declared type (`x: P; x.m()` errors).
   `fun` keeps its types closed by measurement, so subtyping would be a language feature
   (its own ticket if ever wanted); what is ruled here is a matching rule only. A consequence
   of the retraction: the sibling question *"is `x.m()` reachable through a field-only
   annotation"* **dissolves**, because no legal `x : P = U{a = 1}` exists to ask it of.

   Behaviour change the fork must gate and case: today a public method (or public binding)
   hides a record from a field-naming head; after this it does not. The suite decides whether
   anything cased depended on the old behaviour.

   **Landed 2026-09-27** (`d29e437`, merged as `fork/pattern-head-fields-match`; base
   `997b57c`): conformance `892` → **`898`** cases, 0 failed; xUnit `204` → `205`; **no
   existing case moved** — which a pre-merge sweep predicted (no case in the suite combines a
   public-method or public-binding record with a field-naming head). The four table rows above
   re-measured: private binding `1 → 1`, `pub helper` `0 → 1`, `pub method` `0 → 1`,
   `pub impl` `1 → 1`; equality (`x : P = U{a = 1}`) still refused, an extra *field* still
   unmatched without `_`.

   **Implementation, and the one reviewable call.** A `Matching` mode on `MetaContext`,
   set only for the trial in `Elaborator.Matches` and restored in `finally`;
   `Unify.Structs` keeps only `MemberKind.Field` entries while it is set, and the equality
   path never sets it, so unification and member access still see every shown member.
   Coverage (`MatchCompile.Covers`) was already field-only by construction and is untouched.
   The call worth reviewing: a mutable context flag rather than a threaded parameter — it
   reaches every nesting depth without signature churn across eight `Unify.*` partials, at
   the price that anything the trial itself triggers runs in matching mode (undone by the
   trial's `Restore`).

   Measured 2026-09-27, the facts that settle the surrounding questions: a
   structural-record head **does** match a use (`impl Size(struct { a : I64 })` wins
   over `impl Size(_)` at `P{a = 1}`, = 1) because type values are compared
   structurally; record *types* are **closed** (`x : struct { a : I64 } =
   R{a = 1; b = True}` fails `structs with different members`), which is why the
   width must come from the head being a pattern rather than from a type; and a
   structural record type **is** inhabited, via a name-equal literal
   (`P = struct { a : I64 }; x : struct { a : I64 } = P{a = 1}`, = 1). Separately:
   `struct { a = 1 }` is not a record literal at all — it is a struct *type
   descriptor* whose `a` is a member binding (private by default, hence
   `no public member `a``), which is why it never inhabits `struct { a : I64 }`.

   **Blocked on stage 1** of the binder-rule ticket for its plumbing (pattern syntax
   in a head's argument), and it carries two open questions now recorded on this
   ticket: whether a head admits the *whole* pattern grammar or only a struct pattern
   with a rest, and whether field-type binders (`struct { a : p; _ }`) are usable in
   the impl's body.

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
