# STATUS — canonical current implementation snapshot

This is the **authoritative** status document for the `fun` compiler prototype.
When other docs disagree with this file, STATUS.md wins.

Last updated: after the checker evaluation budget, 2026-09-14.

---

## Completed

### Regression coverage
- Imports, module files, records, record patterns, methods, `self`/`Self`, qualified
  patterns/constructors, algebraic effects (`perform`, handlers, `resume`) all have
  regression test coverage. See [regression coverage](wayfinder/topics/regression-coverage.md).

### Type-case / generic programming
- Primitive and nominal type-head matching, structural record type reflection,
  open-`Type` fallback, generic equality dispatch.
  See [type-case / generic programming](wayfinder/topics/type-case-generic-programming.md).

### Record type reflection
- `struct … end` type patterns over constructor fields.
  See [record type reflection](wayfinder/topics/record-type-reflection.md).

### Algebraic effects
- Nominal effect families, latent rows with open row tails, `perform`, match-based
  handlers, `resume`. Deep handler semantics, lexical resume in nested lambdas,
  one-shot continuations. See [algebraic effects](wayfinder/topics/algebraic-effects.md).

### References
- `Ref(A)`, `ref(e)`, `deref(r)`, `r <- e`. Opaque mutable cells, aliasing and
  closure-capture semantics preserved. See [references](wayfinder/topics/references.md).

### Modules and the strict phase rule
- `open <module-expr>` is an item of a module or struct body, not only a `do`-block
  statement: it scopes over the *subsequent* bindings, exports nothing, and carries
  its runtime scope extension as `Core.OpenBind`. Imported modules are strict about
  prelude **syntax** — `Enforest.parse_module` has no `?open_prelude` flag and the
  loader no longer harvests the prelude for them, so a module that uses `+` writes
  `open (import "std")` itself. Prelude *values* still reach a module through the
  importer's elaboration context; see
  [imported module elaboration context](wayfinder/tickets/imported-module-elaboration-context.md).

### Checker evaluation budget (2026-09-14)
- Every evaluation the checker asks for spends from one call budget
  (`Eval_budget`, 1,000,000 calls per request, no surface syntax to raise it);
  running out is `ElabError EvaluationBudgetExceeded`, not a hang.
- A fixpoint unfolds at check time only on a closed argument; a call mentioning
  an unknown variable stays stuck (`HFix` neutral) and costs nothing.
- Running a program (`Ctx.run`, the REPL) is unbudgeted. Macro applications do
  not spend from the budget yet
  ([macro-fuel-is-the-evaluation-budget](wayfinder/tickets/macro-fuel-is-the-evaluation-budget.md)).
  See [checker-evaluation-budget](wayfinder/tickets/checker-evaluation-budget.md).

### Macro model enforcement and one IR (2026-09-14)
- **One IR.** `Surface.t` and lowering are deleted; the elaborator reads expanded
  `Syntax.t`, so ids, paths and spans reach it
  ([delete-surface-ir](wayfinder/tickets/delete-surface-ir.md)).
- **Hygiene.** Every macro application (untyped, type-aware, decl, operator) goes
  through `Expand.application`: use-site and intro scopes on what it receives,
  intro flipped on what it returns. `quote(…)` builds syntax with
  definition-site scopes and holes typed by position (`Expr`/`Pattern`/`Id`).
  Scope sets are opaque `Scopes` values. Macros no longer capture their
  arguments, and template literals resolve at the definition.
- **Reflection is total.** The prelude's `Syntax` ADTs have one constructor per
  form (one `and` chain), and the round trip is the identity on every field.
- **Name resolution.** Local binders always get fresh resolved names
  (`x__0`). A path's head is an id. A bare name resolves to a binder or to an
  **open choice** (the candidate opens by scope set, then the shadowed binder),
  settled by the elaborator against each open's members; no bare name is found
  by spelling among locals. Path heads, traits and effects are still located by
  spelling ([names-resolve-without-spelling](wayfinder/tickets/names-resolve-without-spelling.md)).
- **Macro bodies** elaborate inside the unit opens around their definition,
  nothing ambient (M3). Units that write macros open the prelude themselves.
- **Types.** `type A = … and B = …` chains are mutually recursive nominals. Nested
  patterns through recursive positions work (they read constructors by nominal id).
- Still open from the macro model: explicit `[A]` binders, M5/M8 (one budget,
  error values), M7 (scope-keyed template heads), M9 (templates desugar to
  macros). See the design map's "Macro model distances still open".

### Macro system — Stages 0–10
- Stages 0 through 10 are complete: substrate, hygiene, expansion, phase-aware imports,
  enforestation, syntax templates, computed ADT-based syntax API, kind-tagged macros,
  Decl/Pattern ADTs, type-aware macros. See [macro status](wayfinder/macro-system/STATUS.md).
- The Stage 10 annotation-name disambiguation limitation is resolved on the
  semantic driver path: annotations are resolved against the current prior
  type namespace (builtins, user types, value aliases, and qualified
  imported types via `Expr(M.T)`). See
  [type-aware interleaving](wayfinder/macro-system/TYPE_AWARE_INTERLEAVING.md).
- Type-aware interleaving migration Stages 1–9 are done: AST split, static
  list removal, `Macro_driver` skeleton, prelude-type constraint resolution,
  canonical per-binding kind registration via injected callback,
  macro-generated declaration re-entry (generated `MacroBinding` nodes
  compile/register, generated siblings thread scopes), scoped per-binding
  semantic advancement (top-level source-order prior user type/record
  declarations now constrain later macro annotations), recursive-macro
  safety infrastructure (top-level provisional macro registration/rollback plus
  depth-style macro expansion fuel shared across copied contexts),
  driver-based import loading (`Macro_driver.visit_macros` compiles imported
  public macros through a full driver run, so their annotations resolve in
  the imported module's own context), and retirement of the old
  `Core_loader.visit_macros` parser-heuristic path. Same-Decl generated
  type→macro interleaving and transformer-level self-recursive macro
  bodies are deferred.

---

## Mostly complete / in progress

### Traits and trait stdlib/pub semantics
- Trait declarations, `impl` declarations, structural dictionary evidence,
  trait-bound implicit parameters, qualified method calls, public module/struct
  impl evidence all implemented.
- Remaining: explicit deriving/fallback behavior as library-level type-case code;
  more protocol-style operations. See [traits](wayfinder/topics/traits.md) and
  [trait module/stdlib](wayfinder/topics/trait-module-stdlib.md).

---

## Active / deferred

### Macro Stages 11–12
- Stage 11 (macro-powered language features) and Stage 12 (macro diagnostics &
  expansion UX) have no specification yet. See [macro status](wayfinder/macro-system/STATUS.md)
  and [macro implementation plan](wayfinder/macro-system/IMPLEMENTATION_PLAN.md).

### Annotation scope disambiguation / type-aware interleaving
- Migration Stages 1–9 are done; semantic annotation resolution is active for
  driver-based module compilation and for imported macro modules. Remaining
  deferred items: same-Decl generated type→macro interleaving,
  transformer-level self-recursive macro bodies, mutually recursive macro
  groups, semantic `resolved_type_ref` constraint identity (constraints are
  still recorded by name and resolved at the use site), and resolved-export
  cache fingerprinting (the macro cache is still keyed by module path). See
  [macro interleaving design](wayfinder/topics/macro-interleaving-design.md),
  [type-aware interleaving](wayfinder/macro-system/TYPE_AWARE_INTERLEAVING.md),
  and the [direction map](wayfinder/fun-design-map.md).

### Private type visibility
- Design-only task using the OCaml/SML model (private types become abstract outside
  their defining module). See [private type visibility](wayfinder/topics/private-type-visibility.md)
  and the [direction map](wayfinder/fun-design-map.md).

### Generated symbol cleanup
- Trait declaration markers, struct nominal hashes, and other compiler-internal
  generated symbols should be replaced with explicit structural representations.
  See [generated symbol cleanup](wayfinder/topics/generated-symbol-cleanup.md).

### Enforester improvements
- Structured errors with spans, fault-tolerant parsing, spec-oriented combinators.
  See [enforester improvements](wayfinder/topics/enforester-improvements.md).
