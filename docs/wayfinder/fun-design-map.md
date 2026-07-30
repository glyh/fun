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

## Fog

Dim directions — real, but not yet sharp enough to be tickets. Signposts, cleared
as the frontier reaches them.

- **CLR / C# rewrite shape** — the rewrite is on the active agenda: keep the
  `core_tt` language model, implement the compiler/runtime in C#, target the CLR
  (GC/JIT/tooling for free) rather than a custom VM, preserve room for effects via
  CPS/trampolining. What maps directly from the OCaml prototype and what changes
  structurally is still unknown.
- **Formalized core semantics as cross-rewrite truth** — proposal to write
  `Core.term` / `Core.value` / `eval` (and optionally elaboration) as a Lean 4 (or
  Coq) spec used as an AI-checked structural-correspondence reference across the
  OCaml→C# rewrite. Detail: [formalized-semantics](topics/formalized-semantics.md).
  Hangs on the rewrite shape above.
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
- **Primitive ↔ symbolic-name wiring is stringly-typed and fragile** — a primitive's
  identity is replicated by bare string across three places that must stay in
  lockstep: the runtime reducer table (`nbe_prim.ml` `prim_table`), the type map
  (`elab_prelude.ml` `prims`), and the base-context binding (`elab_entry.ml`, each
  name bound `Var name → HPrim name`) — plus the prelude `stdlib_source` references
  each prim by string literal (`eq_i64(x, y)`, `panic[…]`, …). No single source of
  truth; adding or renaming a prim (e.g. this session's `<` → `lt_i64`) means editing
  every copy by hand, and the "holes" in the stdlib (typed slots the prelude expects
  the compiler to fill) are wired the same brittle way. Want a **maintainable, not
  hacky** scheme — one declaration of `(name, type, reducer)` that the runtime,
  elaborator, base context, and prelude all derive from, with no stringly-typed
  drift. Needs its own investigation of the options (a single registry, generated
  bindings, a typed prim GADT, …) before choosing.
- **Known deferred bug — nested-module ADT constructor resolution** —
  `pub pattern PatWild = RawPatWild(_)` inside a module fails because
  `find_nominal_template_opt` matches by *type* name, not constructor name, on the
  `VNominal` path in `elab_patterns.ml`. Affects pattern matching in macro bodies
  for module-scoped ADTs. Salvaged from the old handover snapshot; revisit when
  macro feature work needs it.

## Open questions

The current frontier — open tickets under [`tickets/`](tickets/), in intended
order. All are unblocked (the ticket that blocked enforester work is now closed).

- [Specify Stage 11 macro-powered language features](tickets/specify-stage-11-macro-powered-language-features.md)
  — umbrella for demoting built-in constructs to library. Direction decided;
  increment 1 (Bool + `if`) landed; **stays open** for more increments.
- [Explicit prelude open for operator demotion](tickets/explicit-prelude-open-operator-demotion.md)
  — make the prelude an explicit `open (import "std")`, deliver operators through
  the `Macro_driver` interleaving (not the static hook/harvest), then move
  `+`/`==`/`<` out of the compiler. **Now unblocked** — its precondition (the table
  unification) landed. The remaining single-live-table + operator-hygiene work
  (deferred by the child) folds into this ticket's interleaving step.
- [Mutually-recursive nominal type declarations](tickets/mutually-recursive-nominal-types.md)
  — language gap: `type A … B …` + `type B … A …` don't elaborate today (only
  self-recursion). Blocks the clean `Branch` ADT below; useful on its own.
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
