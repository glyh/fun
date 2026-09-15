---
title: Fun Compiler Design Map
labels:
  - wayfinder:map
status: open
---

# Fun Compiler Design Map

> **Local-tracker note:** this repo has no `docs/agents/issue-tracker.md`, so the
> map uses the local-Markdown convention. Each ticket is a file under
> [`tickets/`](tickets/); a ticket's **title is its name** and links to its file.
> Blocking is expressed in ticket front-matter (`blocked_by:` lists ticket
> filenames) since there is no native tracker UI. The **frontier** is the open,
> unblocked, unclaimed tickets — see [Open questions](#open-questions).

This map is the single navigation hub for the **direction** of the `fun` compiler:
what has been decided, what is still open, and what is still fog. It is an
**index, not a store** — each decision is gisted here in one line and links to the
topic doc that holds its detail.

## Notes

- **Domain:** `fun`, an experimental dependently-typed language built on `core_tt`
  (bidirectional elaboration, NbE, implicit args, nominal ADTs, structural
  records/modules, traits, algebraic effects, references), with a hygienic
  enforestation-based macro system. Prototype is in OCaml; a CLR/C# rewrite is on
  the agenda (see Fog).
- **Design priority:** Consistency > Flexibility > Correctness. One construct for
  many roles (`struct` = record/module/namespace), types are values, the core
  carries the language model directly. See the root [`README.md`](../../README.md).
- **Source-of-truth split:**
  - **What is *built*** — [`docs/STATUS.md`](../STATUS.md) (overall) and
    [`macro-system/STATUS.md`](macro-system/STATUS.md) (macros). Canonical; when
    other docs disagree on completion status, STATUS wins. These track *state*,
    not direction, and live outside the decision flow on purpose.
  - **How to *work* in the repo** — root [`CLAUDE.md`](../../CLAUDE.md).
  - **What is *decided* / *open*** — this map and its tickets.
  - **Macro reference library** — [`macro-system/`](macro-system/) (design plans,
    papers, extracted Klister notes; its own [`INDEX.md`](macro-system/INDEX.md)).
- **Skills:** prefer `ocaml-alcotest` for test/debug sessions; use `/grilling` and
  `/domain-modeling` when resolving a fog-heavy ticket.
- Do not trust stale "not started / pending" prose in old plan docs when the
  STATUS files disagree — the STATUS files are authoritative.

## Decisions so far

Completed directions. Each line gists the decision; the linked topic doc holds the
detail. (Build-completion status lives in [`docs/STATUS.md`](../STATUS.md).)

### Core type theory & foundations
- [Struct-as-module unification](topics/struct-as-module.md) — one `struct`
  construct serves as record, module, and namespace.
- [Dependent core (`core_tt`)](topics/dependent-types.md) — the old HM pipeline
  was replaced by a dependently-typed core with bidirectional elaboration + NbE.
- [Record types](topics/records.md) — structural record types over constructor fields.
- [Qualified paths](topics/qualified-paths.md) — `M.x` path resolution in the core.
- [Type-specialized equality](topics/type-specialized-equality.md) — equality
  dispatched by type rather than a single generic operator.
- [Pipeline wiring checklist](topics/pipeline-wiring-checklist.md) — the how-to for
  threading a new feature through every pipeline stage (reference/checklist).
- [Impl visibility](topics/impl-visibility.md) — **decided:** impls arrive through
  `open`, matching OCaml's modular implicits, but only after **named impls** land
  as a compile-time handle (`same[A.C, A.eq_C](…)`). Ordering is the decision:
  scoped resolution with no way to name an impl is the one combination no
  precedent uses. Global coherence is unavailable to a language whose modules are
  values.
- [Domain model — elaborate ↔ evaluate](topics/core-tt-domain-model.md) — first
  pass of the port's specification: a context is one sequence seen through
  columns, and the evaluator holds one of them rather than a peer environment.
  *Scope* stays the hygiene word and is not a synonym for it. Vocabulary in the
  root [`CONTEXT.md`](../../CONTEXT.md).
- [Domain model — surface and enforestation](topics/core-tt-domain-model-surface.md)
  (closed) — second pass of the port's specification: the reader commits to
  nothing but grouping, enforestation is parsing interleaved with expansion,
  resolution is sets-of-scopes (largest subset wins, ambiguity loud), expansion
  alpha-renames values only, and lowering is safe only because expand already
  ran. The three macro paths measured against the model's one hygiene contract
  — every divergence a defect ticket; `Surface.t` named as an erasure plus one
  escape hatch, not a language level. Vocabulary in the root
  [`CONTEXT.md`](../../CONTEXT.md).
- [Constructor lookup matches the type name](tickets/constructor-lookup-matches-type-name.md)
  (closed) — premise was stale; the symptom already passed. Pattern-head
  resolution tries the type name first and the constructor name second, and that
  order is now written down in one place instead of hand-inlined in two.
  Also fixed `type T = T I64`: dotted field lookup took the first matching field
  while `open` and `do` bindings take the last, and a type and its constructors
  were bound as scope-set siblings rather than nested. Last-wins is now one named
  helper, `Core.find_field_last`, instead of seven hand-written scans.
- [Core traversals ignore binder depth in binding lists](tickets/core-traversals-ignore-binding-list-depth.md)
  (closed) — instrumented all four cases and ran the suite: never reached. The
  condition that makes them safe is now named and enforced, rather than
  arithmetic being invented for a case that has never occurred.
- [The base context borrows the importer's mutable expander state](tickets/base-context-shared-state.md)
  (closed) — probed every field `unit_base` shares with the importer. The shared
  macro table does not collide (deferred calls carry unit keys, not written
  names), the shared metas are load-bearing, and strictness holds. The one real
  defect, the importer's `expand_ctx` being repointed at the last imported unit's
  expander, is deleted; it was never load-bearing.

### Language features
- [Regression coverage](topics/regression-coverage.md) — approach for locking in
  current behavior; **ongoing**, lowest-risk near-term work.
- [Type-case / generic programming](topics/type-case-generic-programming.md) —
  direct matching on `Type` values; **complete**.
- [Record type reflection](topics/record-type-reflection.md) — `struct … end` type
  patterns over constructor fields; **complete**.
- [Traits / ad-hoc polymorphism](topics/traits.md) and
  [trait module/stdlib semantics](topics/trait-module-stdlib.md) — nominal
  `trait`/`impl`, structural dictionary evidence, bound implicits; **mostly
  complete** — remaining deriving/protocol work is
  [Design trait library deriving and protocols](tickets/design-trait-library-deriving-and-protocols.md).
- [Algebraic effects](topics/algebraic-effects.md) — nominal effect families, open
  rows, `perform`/handlers/`resume`, deep handlers; **complete**.
- [References](topics/references.md) — `Ref(A)`, `ref`/`deref`/`<-`, cell-capturing
  closures; **complete**.
- [Domain model — effects](topics/core-tt-domain-model-effects.md) — fourth pass
  of the port's specification: the vocabulary and E-invariants behind the five
  effect tickets (bare arrow pure, checker budget, lexical handling + handler
  scope, three heap effects with discharge, applicative-by-purity), each marked
  enforced or decided-not-implemented. Vocabulary in the root
  [`CONTEXT.md`](../../CONTEXT.md).

### Macro system
- [Design type-aware macro interleaving handshake](tickets/design-type-aware-macro-interleaving.md)
  (closed & **implemented**) — semantic module driver with an explicit ordered
  queue, split unresolved annotations vs resolved kinds, per-binding semantic
  advancement, provisional recursive-macro registration + fuel, and driver-based
  import loading. Design detail:
  [macro-interleaving-design](topics/macro-interleaving-design.md); the full
  nine-stage migration (Stages 1–9) is implemented — see
  [`macro-system/STATUS.md`](macro-system/STATUS.md).
  Its annotation-resolution rule is superseded by
  [Macro type binders should be explicit](tickets/macro-type-binders-should-be-explicit.md)
  — binders move to `macro m[A](x)`; names in `: Expr(…)` only refer.
- Macro Stages 0–10 (substrate, hygiene, enforestation, syntax templates,
  kind-tagged macros, Decl/Pattern ADTs, type-aware macros) are complete; the
  design library lives in [`macro-system/`](macro-system/).
- [Domain model — macros](topics/core-tt-domain-model-macros.md) (closed) —
  third pass of the port's specification: reflection is **total** and the
  round trip must be the identity (today lossy in both directions — every
  loss ticketed); one hygiene contract governs every application, template
  heads included; macro applications count under the **one evaluation
  budget**, retiring the depth fuel; a macro's name exists from its
  definition's start (provisional), and its body elaborates in its definition
  site's scope, nothing ambient. The dead `phase` field deleted.
- [Bool and `if` as library features](topics/bool-and-if-as-library.md) — Stage 11's
  first increment: `Bool` demoted from a primitive to a prelude nominal ADT
  (`False | True`), primitives return `I64`, and `if` desugared to `match` with
  `Core.If`/`FIf` removed (sound — `FMatch` subsumes them). Implemented, 778 tests
  green. Stage 11 (["macro-powered language features"](tickets/specify-stage-11-macro-powered-language-features.md))
  stays **open** as the umbrella for further demotion increments.
- [Unify procedural macro call syntax with function calls](tickets/unify-macro-call-syntax-with-functions.md)
  (closed & **implemented**, commit `0441c9a`) — `@` removed, macros invoked as
  `f(args)`; macros live in the scope-aware binding table with a Value/Macro kind
  tag (one namespace, innermost-lexical shadowing).
- [Add short-circuit && / || operators](tickets/add-short-circuit-and-or-operators.md)
  (closed & **implemented**) — Stage 11 increment: stdlib `pub infix` templates
  expanding to `match` over Bool (short-circuit for free), seeded like `if`; the
  lexer was simplified alongside: `&`/`|` join `operator_chars` (so `&&`/`||`
  lex by maximal munch, no per-operator rules), redundant rules and the dead
  `At` token removed. Settled line: operator space lexes uniformly, but
  structural punctuation (`|` Bar, `->`, `=`) keeps dedicated tokens.
- [Explicit prelude open for operator demotion](tickets/explicit-prelude-open-operator-demotion.md)
  (closed & **implemented**) — the arithmetic/comparison operators and prefix
  `not` are demoted out of the compiler into the prelude as `pub infix` /
  `pub prefix` declarations (`base_operators()` is just `<-`); the global mutable
  `builtin_syntax_hook` ref and the blanket operator seed are deleted. Prelude
  syntax is delivered under a **strict phase rule**: only where `std` is opened,
  resolved through `load_syntax` on the reserved `import "std"` path (in statement
  order). `Open` carries a module expression; the REPL and program-eval entry
  points open `std` by default. Two loose ends spun out as their own tickets
  (module-level `open`; retire `load_imports_in_terms`/`open_stdlib`).
- [Retire load_imports_in_terms and the open_stdlib survivor](tickets/retire-static-import-harvest-and-open-stdlib.md)
  (closed & **resolved**) — investigated both mechanisms the operator-demotion
  ticket bypassed; both are load-bearing, so both are **kept with documented
  rationale** (no behavior change, 792 tests green). `load_imports_in_terms` can't
  route through the in-order `parse_import` harvest — removing it breaks eager
  circular-syntax-visit detection for template bodies — so it stays as one
  documented use. `open_stdlib` is the persistent ctx-builder counterpart of
  `on_macro_body`'s per-expression `Open (import "std")`; both share
  `open_module_value`, and `Macro_driver`'s advancement hook needs the ctx-builder
  form, so it stays (its dead `ix` return dropped).
- [Module-level open form (strict imported modules)](tickets/module-level-open-strict-imported-modules.md)
  (closed & **implemented**) — `open <module-expr>` is now an item of a module or
  struct body (`Syntax.OpenBinding`, `Core.OpenBind`), scoping over the
  *subsequent* bindings only and exporting nothing. The operator harvest rides on
  the existing in-order `parse_import`; the runtime scope extension shares one
  helper with the expression-level `Open` so de Bruijn indices stay in lockstep
  with `open_module_value`. `Enforest.parse_module`'s `?open_prelude` parameter is
  **deleted**, along with the blanket harvest in `Core_loader` and
  `Macro_driver.visit_macros`: imported modules are now strict about prelude
  **syntax** exactly like expressions, and write `open (import "std")` themselves.
  `parse_expr` keeps the flag — a bare expression has nowhere to write the open.
  The prelude source stays special (`std` must not open itself). 809 tests green.
  One loose end spun out: prelude *values* still reach a module through the
  importer's context (see the ticket below).

## Fog

Dim directions — real, but not yet sharp enough to be tickets. Signposts, cleared
as the frontier reaches them.

- ~~**CLR / C# rewrite shape**~~ — promoted out of fog to
  [port `core_tt` to .NET (F#/C#)](tickets/port-core-tt-to-dotnet.md), now with an
  explicit blocking set. F#-vs-C# is an open question inside that ticket.
- **Formalized core semantics as cross-rewrite truth** — proposal to write
  `Core.term` / `Core.value` / `eval` (and optionally elaboration) as a Lean 4 (or
  Coq) spec used as an AI-checked structural-correspondence reference across the
  port. Detail: [formalized-semantics](topics/formalized-semantics.md). Needs the
  vocabulary from [domain model for `core_tt`](tickets/domain-model-core-tt.md)
  first.
- **Surface syntax after the macro model settles** — whether broad surface syntax
  (possibly less ML-flavored, Ruby/Elixir-style `do … end`) should change once the
  macro expansion model is finalized; the syntax redesign now enters through
  reader/enforestation rather than grammar growth.
- **Diagnostics polish boundary** — which diagnostics work belongs pre-rewrite
  (only to unblock feature work/tests) vs post-rewrite (broad polish, when the
  implementation shape is stable). Broad polish in the OCaml prototype is avoided
  because that code is expected to be replaced.
  Concrete input: elaborator errors carry **no source location at all**. `Elab_error`
  has no span fields. Since delete-surface-ir the elaborator reads `Syntax.t`, so
  every form it sees has a span; attaching them to errors is the remaining work.
- **`Self` names two things** — the record-recursion placeholder
  (`VSelfType args`, set by record declarations) and the partial struct type seen
  by struct method bodies (`elab_infer.ml`, `Struct` case). Both share
  `ctx.self_type`. Giving the record one an identity
  ([self-type-has-no-identity](tickets/self-type-has-no-identity.md)) may force
  them apart; whether they should be one concept remains a naming question —
  the
  [surface domain-model pass](tickets/domain-model-surface-enforestation.md)
  closed without taking it.
- **Library-level features vs compiler machinery** — how much future feature work
  (deriving/fallback, protocol-style ops, UFCS, FFI/native bindings) should be
  library-level macros / type-case rather than new compiler machinery. UFCS and FFI
  are desirable but should not drive the prototype agenda now.
- ~~**Too many IR layers — can one be removed?**~~ — researched in
  [Syntax.t vs Surface.t](tickets/syntax-vs-surface-ir-layer.md). `Surface.t` is
  `Syntax.t` with information thrown away, and it is now deleted (ticket below).
  The other layers (`Raw_syntax`, `Core.term`, values) were not examined.
- ~~**Primitive ↔ symbolic-name wiring is stringly-typed and fragile**~~ — promoted
  out of fog to [one declaration per primitive](tickets/unify-primitive-declaration.md);
  it blocks the port.

## Open questions

### Handover (2026-09-14, end of the M7/M9 run)

State: `main` builds and `dune test` is green (920 tests; the four test binaries
run in ~0.05 / 0.34 / 8.3 / 7.5 s). This run landed the checker evaluation
budget, path heads without spelling, macro fuel into the budget, explicit macro
type binders, NomRef/Con by id, binder counts in `Core.map_subterms`, the brace
surface syntax, M7 (roles by scope set, no mixing) and most of M9 (templates as
macros, expander-driven reading, `Block` token trees, `expand_block`,
idempotent expansion, unwritable resolved names `x#n`) — see `docs/STATUS.md`.

**Recommended next, in order:**

1. **Finish M9** ([ticket](tickets/templates-desugar-to-macros.md), "Left"):
   procedural macro parameter kinds `(n : Id)`, and token-position holes in
   `quote { … }`. Fresh code; the ticket names the mechanism (enforester needs
   parameter kinds while reading a call; loader macro caches carry them).
2. **Small defects**: ~~[capture extents by exceptions](tickets/capture-extents-chosen-by-exceptions.md)~~ (closed),
   [role visibility gaps](tickets/role-visibility-gaps-after-m7.md),
   [term_mentions_var ignores inserted metas](tickets/term-mentions-var-ignores-inserted-metas.md),
   [budget error names no source call](tickets/budget-error-names-no-source-call.md)
   (pairs with attaching spans to elaborator errors, see Fog).
3. **Effects** (decided, unimplemented): [a bare arrow is pure](tickets/bare-arrow-is-pure.md),
   [refs in effect rows](tickets/refs-in-effect-rows.md) (then
   [nominal identity by purity](tickets/nominal-identity-applicative-by-purity.md)),
   [handlers tunnel callback effects](tickets/handlers-tunnel-callback-effects.md).
   `bare-arrow-is-pure` flips every arrow's default — expect wide test churn.

**Needs the user (grilling), do not implement without it:**
[recursive definitions stuck on open arguments](tickets/recursive-definitions-stuck-on-open-arguments.md),
[pattern heads accept type formers](tickets/pattern-head-accepts-type-formers.md),
[what a macro annotation constraint means](tickets/macro-annotation-constraints-mean-nothing.md),
and the older grilling tickets below (struct open, recursive records, `Self`).

**Research, no decision needed yet:**
[deep non-tail recursion is superlinear](tickets/deep-non-tail-recursion-is-superlinear.md)
(OCaml 5 stack scanning; an evaluator design question for the port),
[type-case refinement walks the whole context](tickets/type-case-refinement-walks-whole-context.md).

**Working notes for the next agent:**
- Parallel agents in git worktrees worked well for independent tickets; run
  dune there with `dune build --root .` (the parent `dune-project` otherwise
  captures the build). Worktrees may be created from a stale commit — reset to
  `main` first.
- Use the camlkit MCP tools (locate / uses / type_at) for OCaml navigation.
- Surface syntax changed wholesale: bodies are `{ … }`, arms use `=>`, `;` is
  explicit and a trailing `;` discards. Docs under `macro-system/` and closed
  tickets still show the old `do … end` / `->` syntax; trust the code, `CLAUDE.md`
  and `docs/STATUS.md`.
- Compare performance side by side against a baseline build — the machine may be
  loaded; the M9 regression (125x) was only visible that way.
- When an implementation hits an undecided semantic question, stop and grill
  the user with concrete code examples; several decisions this run were revised
  mid-implementation (generated syntax is hygienic, quotes parse at definition).

The current frontier — open tickets under [`tickets/`](tickets/), in intended
order.

### Blocking the .NET port

[Port `core_tt` to .NET (F#/C#)](tickets/port-core-tt-to-dotnet.md) is the
destination; it is **blocked** until the model exists and the defects a port
would faithfully reproduce are gone. **Blockers extended 2026-09-14:** the decided
effect semantics, the `Self`-identity soundness hole and record knot, the open
semantic questions, and M9's last distance — plus a stability signal (runs that
land without reopening decisions) and two decisions made at port start (no
native-stack recursion in the evaluator; F# vs C#). A port carries code, not invariants — and
every defect below is an invariant with no name in the source.

- [Domain model for `core_tt` before the port](tickets/domain-model-core-tt.md)
  (closed) — the port's specification, written in passes: **all four done** —
  elaborate ↔ evaluate
  ([pass 1](tickets/domain-model-core-tt.md)),
  [surface and enforestation](tickets/domain-model-surface-enforestation.md),
  [macro evaluation and hygiene](tickets/domain-model-macro-hygiene.md), and
  [effects](../topics/core-tt-domain-model-effects.md). The specification is
  written; what remains between the prototype and the port are the defect
  tickets below — each an invariant's distance, none an unnamed rule.
- [Imported modules elaborate in the importer's context](tickets/imported-module-elaboration-context.md)
  (closed) — a compilation unit now elaborates against the base context, so its
  meaning no longer depends on what the importer happened to have in scope, and
  the double-import crash is gone. Macros became members in the same change.
- [Impls and traits extend the context outside the slot list](tickets/bring-impls-and-traits-into-the-slot-list.md)
  (closed) — every binding kind now extends the context through one helper driven
  by the slot list, so both width checks and the drift error are gone. An impl's
  contribution is separated from the evidence that rides along with it.
- [The elaborator's expander handle is named as a context](tickets/expander-handle-is-a-capability-not-a-context.md)
  (closed) — it holds a `macro_runtime`, how to run a macro and how to run it as a
  call under the evaluation budget, instead of a borrowed expander. One adapter is where the two libraries
  meet.
- [Elaborator and evaluator agree on binding-list env width only by parallel arithmetic](tickets/env-width-contract-is-unnamed.md)
  (closed) — a binding's contribution is now one ordered slot list in
  `Core.binding_slots`, pushed by the evaluator and zipped by the elaborator, so
  order and count exist once instead of three times. Width is its length. The
  evaluator's two binding folds became one. Remainder is its own ticket below.
- [One declaration per primitive](tickets/unify-primitive-declaration.md)
  — two hand-synced tables plus prelude source strings. The ~10-line assertion is
  **done** (a typed primitive with no reducer now aborts at startup), as is the
  division-by-zero leak it turned up. Open only for the unification question,
  which should wait until that check actually fires.
- [Struct open does not scope over `con_fields`](tickets/struct-open-does-not-scope-over-con-fields.md)
  (closed) — implemented 2026-09-15: struct items in source order (a field is an item), fields see earlier opens and bindings, methods checked after the last field, a field type mentioning a method is a cycle error.
- [Block-local macros leak by written name](tickets/block-local-macros-leak-by-written-name.md)
  (closed) — the written-name fallback now serves only context-less
  (string-built) ids, so a source-written call resolves by scope set alone and a
  block's macro is not reachable after it.

### Language and macro work

- [Specify Stage 11 macro-powered language features](tickets/specify-stage-11-macro-powered-language-features.md)
  — umbrella for demoting built-in constructs to library. Direction decided;
  increment 1 (Bool + `if`) landed; **stays open** for more increments.
- [Mutually-recursive nominal type declarations](tickets/mutually-recursive-nominal-types.md)
  (closed) — `type A = … and B = …` chains, elaborated by one three-phase knot.
  Nested patterns through recursive positions (broken for every recursive type,
  `List` included) now read a placeholder's constructors by nominal id.
- [Mutually-recursive record type declarations](tickets/mutually-recursive-record-types.md)
  — grilled 2026-09-15: identity at the knot (Go-style); a recursive record mints an id, `Self` carries it and unfolds on demand; same-shape recursive records are distinct. Implement with the two tickets below.
- [Recursive records cannot hold a record](tickets/recursive-records-cannot-hold-a-record.md)
  — `Som(l)` in a recursive field fails; `Self` is never unfolded. Direction
  depends on the knot decision.
- [Self type has no identity, so unrelated recursive records unify](tickets/self-type-has-no-identity.md)
  — latent soundness hole; becomes live if the bug above is fixed alone.
- [Mutual type chains in scoped do-heads](tickets/type-def-chains-in-scoped-do-heads.md)
  — deferred remainder of the nominal ticket: `and` chains rejected in scoped
  `do type … ; body` heads until expression-position group knots are designed.
- [Reflect Match in the Expr macro ADT](tickets/reflect-match-in-expr-macro-adt.md)
  — enables a true prelude-macro `if`, `matches?`, pattern DSLs, `derive`. Design
  decided via grilling; unblocked (mutual nominal types landed).
- [Specify Stage 12 macro diagnostics and expansion UX](tickets/specify-stage-12-macro-diagnostics-and-expansion-ux.md)
  — diagnostics scope pre- vs post-rewrite.
- [Design trait library deriving and protocols](tickets/design-trait-library-deriving-and-protocols.md)
  — library-level deriving/protocol ops for traits. Now carries two inputs from
  [impl-visibility](topics/impl-visibility.md): deriving inherits the
  mandatory-open tax, and scoped impl resolution gives up coherence, which any
  ordered or hashed collection would inherit as a soundness hazard.
- [Design private type visibility model](tickets/design-private-type-visibility-model.md)
  — decision on private/opaque type visibility.
- [Scope generated symbol cleanup](tickets/scope-generated-symbol-cleanup.md)
  — when to replace generated compiler symbols with structural forms.
- [Scope enforester improvements](tickets/scope-enforester-improvements.md)
  — which enforester improvements to do pre-rewrite.

### IR layers (from [Syntax.t vs Surface.t](tickets/syntax-vs-surface-ir-layer.md))

- [Type-aware macro output is not expanded](tickets/type-aware-macro-output-is-not-expanded.md)
  (closed) — both type-aware call sites share one helper that expands the output
  before lowering; a non-syntax result is an error naming the macro, not a hole.
- [Procedural macros capture use-site variables](tickets/procedural-macros-capture-use-site-variables.md)
  (closed — one application contract, use-site + intro scopes) — **diagnosed** (pass-2 model): the `Syntax.t` ↔ value round-trip drops scope
  sets (`value_to_id` hardcodes empty), so a spliced argument loses its
  occurrence scope and the macro's binder captures it. Templates' splices are
  clean — their written literals are the remaining hole:
- [The syntax round trip is lossy](tickets/syntax-round-trip-is-lossy.md)
  (closed — reflection total, round trip the identity) — **found by pass three**: the round trip loses scope (at both ends), type
  annotations, explicitness, `PatCon` paths and span positions, and degrades
  silently (`?` ids, garbage→`False`, skipped bindings). The general defect
  that the capture ticket is the soundness slice of.
- [Template literals resolve at the use site](tickets/template-literals-resolve-at-use-site.md)
  (closed — resolved names always fresh, path heads are ids, one template instantiation)
  — a use-site `False = 42` silently turns the prelude's own `&&` into a
  constant-42 machine; replacement ids are re-enforested at the use site and
  the declarer's scope never reaches them. Found by the pass-2 model.
- [Macros have no quoted syntax](tickets/macros-have-no-quoted-syntax.md)
  (closed — `quote(…)` with typed holes; bare names resolve to binders or open choices)
  — ids can only be built from strings with empty scope sets, so the
  implementation resolves them by spelling at elaboration. The model: quoted
  syntax resolves at the definition site; a context-less id is unbound.
- [Macro bodies implicitly open the prelude](tickets/macro-bodies-implicitly-open-the-prelude.md)
  (closed — a body is elaborated inside the unit opens around its definition)
  — `Macro_driver` elaborates every macro body prelude-opened; the model
  elaborates it in its definition site's scope, nothing ambient.
- [Macro type binders should be explicit](tickets/macro-type-binders-should-be-explicit.md)
  (closed) — `macro m[A](x) : Expr(A)`; names in an annotation only refer, and an
  unbound one is an error at the definition.
- [Delete Surface.t; elaborate expanded Syntax.t](tickets/delete-surface-ir.md)
  (closed) — one IR: the elaborator reads expanded `Syntax.t`, so spans and ids
  reach it.

### Macro model distances still open (from the 2026-09-14 implementation run)

The macro model's M1–M3, M5, M6, M8, M10–M12 and most of M2 are now enforced.
What remains, in the recommended order:

- [Path heads, traits and effects resolve without spelling](tickets/names-resolve-without-spelling.md)
  (closed) — path heads carry an open choice like bare names; traits and
  nominals are located through the entry a head resolves to, and the
  name-keyed trait table and environment scans are gone.
- [Macro type binders should be explicit](tickets/macro-type-binders-should-be-explicit.md)
  (closed, listed above).
- [Macro fuel is the evaluation budget](tickets/macro-fuel-is-the-evaluation-budget.md)
  (closed) — M5 + M8: a macro application is a call under the one budget, its
  body and output spending from the same request; expansion failures are
  `Expand_error` values.
- [Template and operator heads resolve by scope set](tickets/template-heads-resolve-by-scope-set.md)
  (closed & **implemented**) — M7: raw tokens carry scope sets, a role resolves
  by scope set (largest subset), definition contexts scope their tokens, and
  generated syntax is hygienic (a use-site name via a name-position hole). A
  role never mixes with another binder of its name — `RoleConflict` in either
  order, `OpenSuppliesRole` at an open — exempting application-written binders
  and fixity attached to its value. The expander-driven loop and unparsed block
  captures ride on M9; the region rule stays until then.
- [Surface syntax — brace bodies, `=>` arms, explicit semicolons](tickets/surface-syntax-braces.md)
  (closed & **implemented**) — decided with M7: `fn(x) { … }`, `if (c) { … }`, `match (v) { | p => e }`,
  `struct { x: I64 }` record types, `->` only for function types, newlines are
  whitespace, trailing `;` discards.
- [Templates desugar to macros](tickets/templates-desugar-to-macros.md) — M9
  (closed & **implemented**). **Run 3 (2026-09-15):** macro parameters take kinds
  — `(n : Id)`, `(p : Pattern)`, `(b : Block)`, `(d : Decl)` a brace group of
  items whose value is `Decls` — read by the enforester at the call and carried by
  the loader's caches; a token-position `$n` in `quote { … }` names a generated
  rule's head; a declaration hole splices `Decls`.
  **Run 2 (2026-09-14): implemented** — rules as reflected data, one
  instantiation path through `Expand.application`, bodies and items read form by
  form as expansion reaches them, exports from expansion, `Block` token trees,
  `expand_block`, idempotent expansion, unwritable resolved names (`x#n`).
  **Run 1 (2026-09-14):** kinds as types, `quote { … }`, `: Decl` templates and
  `multi`'s removal landed; stopped on five questions in the ticket (bodies inside
  quotes vs typed holes, template macros and the elaborator, where syntax exports
  come from, syntax declarations as data, idempotence). **Grilled 2026-09-14:** `quote { … }` for declarations, `$name` resolves to its
  nearest binder (no quote levels), kinds spelled `$(x : Decl|Id|Pattern|Block)`,
  templates take `: Decl` and `multi` is deleted. Unparsed bodies grilled: `Block` is a readable token tree placeable in any `{…}` slot, bodies stay raw until expansion reaches them, `expand_block` is local-expand, expansion is idempotent. Unblocked (M7 landed); now also
  carries M7's expander-driven loop and unparsed block captures. Retires the
  template region rule in `add_id_scope_if`.

### Found by the budget and path-heads run (2026-09-14)

- [Recursive definitions stuck on open arguments](tickets/recursive-definitions-stuck-on-open-arguments.md)
  — grilled 2026-09-15: recursive calls unfold on open arguments under the checker budget; exhaustion is an error (`HFix` stuck rule goes).
- [Pattern heads accept type formers](tickets/pattern-head-accepts-type-formers.md)
  — (closed) intended: a function reducing to a nominal works as an alias in a pattern head.
- [Elaborator matches `EffectRow` and `stx_` by spelling](tickets/elaborator-matches-names-by-spelling.md)
  — (closed) both dispatches deleted; a type named `EffectRow` now means itself.
- [The closed-term rule does not look inside closures](tickets/stuck-rule-ignores-closure-environments.md)
  (closed) — a closure is closed when the slots its body reads are; an `open` in the body is conservatively not.
- [A budget error names a core term](tickets/budget-error-names-no-source-call.md)
  (closed) — names the fixpoint, the demanding request and the form's span:
  `calling loop, in an evaluation while reading the type at <file>:1:57-1:64`.
- [Deep non-tail recursion is superlinear](tickets/deep-non-tail-recursion-is-superlinear.md)
  — pre-existing and quadratic: OCaml 5 minor GCs rescan the whole native stack. Architectural (the evaluator recurses natively per object-language call).
- [A qualified constructor nested in a pattern argument is unknown](tickets/nested-qualified-constructor-pattern.md)
  (closed) — root cause was `NomRef` finding a nominal by spelling; it now
  carries the nominal id.
- [A quoted constructor value is re-found by spelling](tickets/quoted-constructor-values-by-spelling.md)
  (closed) — `Con` deleted; a `VCon` quotes to the `Ctor` term carrying its nominal.
- [Expansion errors reach the user raw](tickets/expansion-errors-reach-the-user-raw.md)
  (closed) — an overrun inside an application is its `Expand_error` with the
  operator's site; evaluator errors (`panic`, …) in a body still carry no site.
- [An evaluation error inside a macro body carries no site](tickets/macro-body-eval-errors-lack-site.md)
  (closed) — the evaluator fails through one helper that raises the running
  application's error, so a `panic` in a macro body carries the operator span.
- [Each core-term traversal counts binders on its own](tickets/core-traversals-count-binders-separately.md)
  (closed) — live in generalization's closedness check; every traversal now reads `Core.map_subterms`.
- [term_mentions_var ignores inserted metas](tickets/term-mentions-var-ignores-inserted-metas.md)
  (closed) — it now mentions each slot its mask binds; dependent application no
  longer applies such a codomain to an out-of-scope variable.
- [What a macro annotation constraint means](tickets/macro-annotation-constraints-mean-nothing.md)
  — `: Expr(I64)` resolves but is unused; one type binder max fell out of the implementation. Grill.

### Found by the M7 and M9 implementations (2026-09-14)

- [Brackets decide grouping](tickets/brackets-decide-grouping.md)
  — adopt Rhombus's structural extents without layout: `{}`/`[]`/`()` group, a
  hole never ends at a bare keyword; Rust-style arms (`A | B => e,`), `|` only
  union; relative precedence in named, transitive order groups; a non-trailing
  hole is one term. Grilled and implemented 2026-09-15 (arms; order groups and
  hole extents on `order-groups`, with `capture-extents` merged in). Open: tail-
  returning forms, dotted group references.
- [Role visibility gaps left by M7](tickets/role-visibility-gaps-after-m7.md)
  (closed) — an import's roles bind in the region of the open or binder that
  imported it; `Import` carries its written scope set, so every open is checked
  against the roles visible where it is written, driver-run opens included.
- [Capture extents are chosen by catching parse errors](tickets/capture-extents-chosen-by-exceptions.md)
  (closed) — a capture reads as far as its parser does, one parse per hole; the
  hole ending a use reads at the form's precedence (`inc 1 * 10` is
  `(inc 1) * 10`); whitespace never ends a capture.
- [Type-case refinement walks the whole context per branch](tickets/type-case-refinement-walks-whole-context.md)
  — the remaining elaboration hotspot after the M9 performance fix.
- Struct items are read together with a private copy of the roles, not form by
  form (M9 run 2 choice; their bodies still wait for expansion).

### Found by the domain-model audit (2026-09-15)

- [Records are declared only by let bindings](tickets/records-only-let-bindings.md)
  — `type X = struct { … }` deleted; a `rec` struct binding mints the recursive
  record's identity; needs value-level `rec … and …`.
- [ADTs are declared by let bindings](tickets/adts-as-let-bindings.md)
  — `Option = fn(A : Type) { enum { Some(A), None } }`; `type` deleted.
  **Blocked on** nominal identity applicative by purity (E11).
Invariant distances the audit found with no ticket (re-verified on `fa2f32d`).

- [Declaration binders keep their written name](tickets/declaration-binders-keep-written-names.md)
  — M12/S5: types, constructors, traits, effects and module items resolve under
  their spelling; the elaborator's macro table is string-keyed. Grill: which
  binders get fresh names.
- [A resolved name can be forged](tickets/resolved-names-forgeable.md)
  — M11/M12: any `#` name is trusted as resolved (`new_id("x#5")`); empty-scope
  and operator macro heads fall back to spelling. Grill: does `new_id` stay.
- [Opening a module needs its value](tickets/module-open-width-depends-on-value.md)
  — I2: open width zips type and value entries; effect collection skips a
  non-module open silently.
- [Dotted paths found by first match](tickets/dotted-paths-first-match.md)
  — I3/M12: `Elab_stdlib.resolve` and named-impl lookup take the first member;
  prelude nominal names are literals outside `Compiler_names`.
- [The elaborator's macro runtime is mutable](tickets/elaborator-macro-runtime-is-mutable.md)
  — I4e: with `base = None` an import overwrites the importer's handle.
- [An under-applied Decl macro is fed a dummy Unit](tickets/decl-macro-fed-dummy-unit.md)
  — M8: `force_val` fabricates arguments; arity is checked only for kinded calls.
- [Refresh the domain-model docs' "today" sections](tickets/refresh-domain-model-today-sections.md)
  — stale descriptive prose in the core, macro and surface topics.

### Effects (from the [domain-model pass](topics/core-tt-domain-model-effects.md))

Decided by the effects domain-model pass; unimplemented, each ticket is the
distance from an E-invariant in the topic doc.

- [Refs belong in effect rows](tickets/refs-in-effect-rows.md) — mutation is
  the three heap effects `Alloc(h)`/`Read(h)`/`Write(h)` over branded
  `Ref(h, A)`; discharge at generalisation. Today refs are invisible in types.
- [Nominal identity is applicative by purity](tickets/nominal-identity-applicative-by-purity.md)
  — same declaration + convertible free variables = same type; generative only
  under a run-time effect. Today a nominal declared under a binder does not
  evaluate at all. **Blocked on** refs in effect rows.
- [A bare arrow is pure](tickets/bare-arrow-is-pure.md) — `A -> B` is pure;
  `A -> B can _` / `A ~> B` infers the row. Reverses effects Phase 6's default.
- [The checker evaluates under a budget](tickets/checker-evaluation-budget.md)
  (closed & **implemented**) — each checker request to the evaluator spends
  from one call budget, and exhaustion is an elaboration error; a fixpoint
  applied to an open argument stays stuck (`HFix`). Running a program is
  unbudgeted. `loop(0)` in a type is now a budget error, not a hang.
- [Handlers tunnel callback effects](tickets/handlers-tunnel-callback-effects.md)
  — lexical handling: a callback's effects pass handlers in code polymorphic
  over its row, and an effectful closure may not escape its handler's scope.
  Today a library's internal handler swallows the user's raise.
