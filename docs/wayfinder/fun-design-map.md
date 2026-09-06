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

### Macro system
- [Design type-aware macro interleaving handshake](tickets/design-type-aware-macro-interleaving.md)
  (closed & **implemented**) — semantic module driver with an explicit ordered
  queue, split unresolved annotations vs resolved kinds, per-binding semantic
  advancement, provisional recursive-macro registration + fuel, and driver-based
  import loading. Design detail:
  [macro-interleaving-design](topics/macro-interleaving-design.md); the full
  nine-stage migration (Stages 1–9) is implemented — see
  [`macro-system/STATUS.md`](macro-system/STATUS.md).
- Macro Stages 0–10 (substrate, hygiene, enforestation, syntax templates,
  kind-tagged macros, Decl/Pattern ADTs, type-aware macros) are complete; the
  design library lives in [`macro-system/`](macro-system/).
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
  struct body (`Syntax`/`Surface.OpenBinding`, `Core.OpenBind`), scoping over the
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
- **Library-level features vs compiler machinery** — how much future feature work
  (deriving/fallback, protocol-style ops, UFCS, FFI/native bindings) should be
  library-level macros / type-case rather than new compiler machinery. UFCS and FFI
  are desirable but should not drive the prototype agenda now.
- **Too many IR layers — can one be removed?** — the pipeline carries several
  intermediate representations: `Raw_syntax` → `Syntax.t` (enforest) → `Surface.t`
  (lower) → `Core.term` (elaborate) → `value` (NbE). This is more layers than may be
  needed. Suspected prime candidate: `Syntax.t` and `Surface.t` appear close to
  **isomorphic** — `lower_surface.ml` / `surface_to_syntax.ml` are near 1:1
  structural maps with no desugaring — so one of them might be collapsible. Needs
  its own investigation (what each layer actually buys: hygiene/scope-set carriage,
  macro reflection boundaries, the two desugaring seams at enforest and elaborate)
  before any merge decision. Do **not** pre-slice into tickets yet.
- ~~**Primitive ↔ symbolic-name wiring is stringly-typed and fragile**~~ — promoted
  out of fog to [one declaration per primitive](tickets/unify-primitive-declaration.md);
  it blocks the port.

## Open questions

The current frontier — open tickets under [`tickets/`](tickets/), in intended
order.

### Blocking the .NET port

[Port `core_tt` to .NET (F#/C#)](tickets/port-core-tt-to-dotnet.md) is the
destination; it is **blocked** until the model exists and the defects a port
would faithfully reproduce are gone. A port carries code, not invariants — and
every defect below is an invariant with no name in the source.

- [Domain model for `core_tt` before the port](tickets/domain-model-core-tt.md)
  — the port's specification. First pass scoped to the elaborate ↔ evaluate
  boundary, where most of the unnamed invariants live. **Do this first.**
- [Imported modules elaborate in the importer's context](tickets/imported-module-elaboration-context.md)
  — importing one module twice crashes whenever its body mentions a name it did
  not bind itself. Demonstrated, with controls, in the ticket.
- [Elaborator and evaluator agree on binding-list env width only by parallel arithmetic](tickets/env-width-contract-is-unnamed.md)
  — the de Bruijn contract written twice, in two libraries, unnamed and unchecked.
- [One declaration per primitive](tickets/unify-primitive-declaration.md)
  — two hand-synced tables plus prelude source strings. Ticket now records why
  the obvious refactor resists (`panic` does not fit a `(name, type, reducer)`
  record) and a ~10-line assertion that captures most of the value first.
- [Constructor lookup matches the type name, not the constructor name](tickets/constructor-lookup-matches-type-name.md)
  — confirmed live: `find_nominal_template_opt`'s env scan compares `n.name`.
- [Core term traversals ignore binder depth in binding lists](tickets/core-traversals-ignore-binding-list-depth.md)
  — the same env-width invariant broken in a third place. Latent today.
- [Struct open does not scope over `con_fields`](tickets/struct-open-does-not-scope-over-con-fields.md)
  — settle the rule before the struct elaborator is written a second time.

### Language and macro work

- [Specify Stage 11 macro-powered language features](tickets/specify-stage-11-macro-powered-language-features.md)
  — umbrella for demoting built-in constructs to library. Direction decided;
  increment 1 (Bool + `if`) landed; **stays open** for more increments.
- [Mutually-recursive nominal type declarations](tickets/mutually-recursive-nominal-types.md)
  — language gap: `type A … B …` + `type B … A …` don't elaborate today (only
  self-recursion). Blocks the clean `Branch` ADT below; useful on its own.
- [Mutually-recursive record type declarations](tickets/mutually-recursive-record-types.md)
  — deferred remainder of the nominal ticket: records excluded from `and` chains
  until a value-level knot mechanism is designed.
- [Mutual type chains in scoped do-heads](tickets/type-def-chains-in-scoped-do-heads.md)
  — deferred remainder of the nominal ticket: `and` chains rejected in scoped
  `do type … ; body` heads until expression-position group knots are designed.
- [Reflect Match in the Expr macro ADT](tickets/reflect-match-in-expr-macro-adt.md)
  — enables a true prelude-macro `if`, `matches?`, pattern DSLs, `derive`. Design
  decided via grilling; **blocked on** mutually-recursive nominal types (for the
  dedicated `Branch` ADT).
- [Specify Stage 12 macro diagnostics and expansion UX](tickets/specify-stage-12-macro-diagnostics-and-expansion-ux.md)
  — diagnostics scope pre- vs post-rewrite.
- [Design trait library deriving and protocols](tickets/design-trait-library-deriving-and-protocols.md)
  — library-level deriving/protocol ops for traits.
- [Design private type visibility model](tickets/design-private-type-visibility-model.md)
  — decision on private/opaque type visibility.
- [Scope generated symbol cleanup](tickets/scope-generated-symbol-cleanup.md)
  — when to replace generated compiler symbols with structural forms.
- [Scope enforester improvements](tickets/scope-enforester-improvements.md)
  — which enforester improvements to do pre-rewrite.
