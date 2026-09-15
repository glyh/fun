# STATUS — canonical current implementation snapshot

This is the **authoritative** status document for the `fun` compiler prototype.
When other docs disagree with this file, STATUS.md wins.

Last updated: after fresh declaration binders and unforgeable resolved names, 2026-09-15.

---

## Completed

### Fresh declaration binders, no string-built ids (2026-09-15)

- Every declaration binder - types, constructors, effects, traits, pattern
  synonyms, module items, macros - gets a fresh resolved name
  (`Expand.bind_declaration`); a declaration exports its label (`Syntax.label`),
  and the elaborator keys its context by resolved name. A macro's `type Tmp`
  no longer takes the caller's `Tmp`.
- `Syntax.new_id`, `var`, `lam`, `let_in`, `seq` and `no_scopes` are deleted: a
  macro gets a name from `quote` or an `Id` parameter. A reflected id's `Scopes`
  carries the resolved name it was minted with, and a reflected `#` name is
  accepted only under that certificate, so a macro cannot forge one. The
  empty-scope and operator spelling fallbacks for macro heads are gone.

### Performance after M9 (2026-09-14)
- M9 run 2 slowed the suites up to ~125x (`test_macro_driver_stage7`
  0.13s → 16s). Two causes, both fixed without semantic change:
  - **Cubic item loop.** A definition context re-applied every active scope to
    *all* its remaining unread items before each item. An item's extent is
    structural (`take_statement`), so only the item being read takes the
    scopes; several scopes are added as one union, not one traversal each.
  - **Context refinement copied the whole context.** `refine_context_type_var`
    (type-case branches) was ~90% of `init_ctx` before M9 too, and grew with
    the larger prelude. The substitution now preserves sharing and memoises
    values and environment tails by physical identity.
- Side by side with `0262a02`: stage7 0.07/0.05s, syntax 0.30/0.34s,
  elaborate 7.5/8.3s, core 6.9/7.5s.

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

### Brace surface syntax (2026-09-14)
- Bodies are brace groups: `fn(x) { … }`, `method m() { … }`, `macro m(x) : K { … }`,
  `infix (op) g ($a, $b) { … }`, blocks `{ … }`, `module { … }`,
  `sig { … }`, `struct { … }`, `multi { … }`, `syntax head { rule => replacement, … }`.
- `if (c) { t } else { e }` (a prelude template, `if ($c) $t else $e`) and
  `match (v) { pattern => result, effect E.op x => result }` (Rust-style arms: a `{ … }` result ends its arm, any other ends at `,`; `|` is pattern union).
- `->` is only the function-type arrow; `=>` separates a pattern from its result
  and is reserved (`infix (=>)` is an error).
- Record types are `type P = struct { x: I64 }`; construction, record patterns and
  `can {…}` rows are unchanged. `module M do … end` is gone (`M = module { … }`).
- Newlines are whitespace; `;` separates. A trailing `;` before `}` discards the
  block's value, and a block statement may be a bare expression (`_ = e`).
- Old `-> body` / `do … end` / `match x do` forms fail with an error naming the
  new form. The reader's keyword-pair grouping helpers are deleted; arms split
  by "a pattern holds no bare `=>`, a result holds no bare `|`".
- A template hole ending a group now extends its capture to the whole group.
- Prelude and every test source migrated mechanically
  ([surface-syntax-braces](wayfinder/tickets/surface-syntax-braces.md)).

### Struct items in source order (2026-09-15)

- A struct's items are one source-ordered list: a field is an item
  (`Syntax.FieldBinding`, reflected `DeclField`; `RawStruct` carries only items).
- A field's type sees the opens and bindings written before it, as a module's
  items do; a later binding is not visible. Its type leaves the struct as a
  value, quoted at the struct's own level.
- A method is checked after the last field, so `self` has every field. A field
  type mentioning an earlier method is `FieldTypeMentionsMethod` (a cycle).
- `Self` in an item other than a field or method is the fields written so far.
- No dependent fields: a field is not a binder, so `struct { n : Type; v : n }`
  leaves `n` unbound. A struct does not see its own name (`C` binds after
  `C = struct { … }`); `C.k` inside it is unbound.

### Macro signatures (2026-09-15)
- A macro's type binders, `(x : Expr(T))` parameters and `: Expr(T)` output are
  its **signature**, a pi type elaborated where the macro is defined
  (`Syntax.macro_signature`, carried on `Expand_ctx.macro_entry`). A name in it
  must resolve there and a promised `T` must be a type. The `_ = T` body device is
  deleted; `MacroBinding`/`MacroDef` carry `output`, reflected both ways.
- A macro whose signature promises a type waits for the elaborator, which applies
  it like a function over types (`Elab_resolve.apply_typed_macro`): binders become
  metas, typed arguments are checked at their types, the result meets the
  expected type, every binder must then be solved ("cannot infer A for
  `default`"), the macro runs with them, and its output is checked at the promised
  type ("macro `n` promises Expr(I64) …"). Any number of binders. A macro that
  promises no type still runs during expansion.
- A typed argument elaborates once, at the call. Where the output places it
  unchanged it becomes `Syntax.Elaborated` (internal, reflected as the argument
  itself), and elaborating that reuses the core, weakened past the binders the
  output added (`Elab_defs.shift_term`, which now widens inserted metas' masks).
  A rebuilt argument is new syntax and elaborates normally; an argument whose
  core holds an `open` placed under new binders elaborates again.
- A typed call's effects are its output's: the elaborator records the output by
  the call's node (`Elab_resolve.deferred_outputs`) and the effect pass reads it
  there. Every effect read follows the elaboration it reads — a type is required
  pure after it elaborates and before it is evaluated
  (`Elab_type_expr.require_pure`), so a typed call works in a type position too.
- `open` of a non-module is `NotAModule` in every form (expression, module item,
  struct item, effect pass); an open's entries are its module type's public
  fields and impls, and a value whose entries do not line up with its type is an
  internal invariant failure, never a silently narrower open.
- The elaborator's copied macro table is gone: it asks its macro runtime, so a
  typed macro works inside the unit that defines it and when imported
  ([macro-annotation-constraints-mean-nothing](wayfinder/tickets/macro-annotation-constraints-mean-nothing.md)).

### Order groups and structural hole extents (2026-09-15)
- Precedence is relative: `order g : stronger_than(a) weaker_than(b) assoc(right)`
  declares a group (a binder resolved by scope set, `pub`, delivered by `open` and
  import binders); the order is transitive and a cyclic declaration is an error.
  `infix (op) g`, `prefix (op) g`, `syntax name g { … }` join a group; numeric
  precedence is gone (an error names the new form). Operators with no declared
  order never mix ("no declared order; parenthesise"); a form or operator in no
  group is weaker than every grouped one. The prelude's operators are in
  `disjunction < conjunction < comparison < additive < multiplicative < negation`.
- A syntax form's hole extent is structural: the hole ending a use reads the
  form's operand at its order; a hole before `,`/`;` reads to it; any other hole
  is one term (a token or one bracket group). Captures are one parse per hole
  (`try_prefixes` deleted). See
  [brackets-decide-grouping](wayfinder/tickets/brackets-decide-grouping.md).

### Macro model M9, run 3 — M9 complete (2026-09-15)
- A macro parameter takes a kind: `macro m(n : Id, p : Pattern, b : Block, d : Decl)`.
  A call's arguments are read as the kinds of the macro its head resolves to,
  local or imported (the loader's caches carry the kinds); a `Decl` argument is a
  brace group of items, unread until spliced, and its value is `Decls`. A wrong
  kind or count is an `Expand_error`.
- A declaration hole `$d` in `quote { … }` takes `Decls` and splices them.
- A `$n` identifier token in `quote { … }` (a generated rule's head) is an `Id`
  hole.
- An import's roles bind in the region of the open or binder that imported it.

### Macro model M9, run 2 (2026-09-14)
- A syntax form is a macro: its rules are reflected data on its role, and a use
  is filled through `Expand.application` (`Instantiate`). The region rule and
  token-level re-enforestation are gone.
- The expander drives the enforester: `{ … }` bodies and unit/module items stay
  unread (`Block`, `Items`) until expansion reaches them, so syntax a form
  generates is usable by the next form. Quoted syntax is read where written.
- A unit's syntax exports come from expanding it.
- `Syntax.tokens(b)` reads a block's token tree; `Syntax.expand_block(b)` expands
  one inside a macro; expansion is idempotent. Resolved names are `x#n`.
- A `: Decl` form works as a block statement; binders a macro returns into a
  definition context lose the use-site scope.
- Test changes: units that define forms open `std` themselves (a replacement is
  read at its definition); import cycles are reported by the syntax load that
  reaches them first; the circular-syntax tests use real import cycles.

### Macro model M9, run 1 (2026-09-14)
- Hole kinds are reflection types: `$(x : Expr | Block | Id | Decl | Pattern)`,
  a bare `$v` is `Expr`; `binder`/`ident` are gone (`Id` binds or refers by
  position). A `Pattern` capture splices a use-site pattern.
- `syntax head : Decl { pat => { items } }`; a syntax form is used only in its
  kind's position. `multi` is deleted.
- `quote { items }` quotes declarations; a lone `$d` item is a `Decl` hole.
- The expansion position is the site's: a `Decl` macro works inside an
  expression-level `module { … }`.
- (Run 2 below lands templates as macros, the loop, `Block` and `expand_block`.)

### Checker evaluation budget (2026-09-14)
- Every evaluation the checker asks for spends from one call budget
  (`Eval_budget`, 1,000,000 calls per request, no surface syntax to raise it);
  running out is `ElabError EvaluationBudgetExceeded`, not a hang.
- A fixpoint unfolds at check time on any argument, open or closed (revised
  2026-09-15, see below): `double(n)` converts with `n + n`; a divergent
  unfolding such as `loop(n)` in a type is a budget error naming the call.
- The budget measures work: calls plus every conversion and unification step.
  Two calls of one known-pure fixpoint (closed empty effect row) on convertible
  arguments convert without unfolding: under the checker such a call is a
  deferred `VGlued`, unfolded when inspected (lazy delta).
- **One binder count per core form.** `Core.map_subterms` states how many
  environment entries each immediate subterm sits under (`None` where only
  evaluation knows: an `open`'s body, bindings after an `OpenBind`); the
  closed-term rule, `shift_term`, generalization's closedness check,
  `term_mentions_var` and the recursive-payload closer all read it. This fixed
  generalization under match branches and around `perform`
  ([core-traversals-count-binders-separately](wayfinder/tickets/core-traversals-count-binders-separately.md)).
- Running a program (`Ctx.run`, the REPL) is unbudgeted.
  See [checker-evaluation-budget](wayfinder/tickets/checker-evaluation-budget.md).
- **Macro applications are calls under the same budget** (M5). The depth fuel
  (256, reserve/release) is deleted. A macro application spends one call and
  opens a request that its body evaluation (fresh metas, shared budget,
  `Nbe.apply_macro`) and the expansion of its output spend from, so a nest of
  applications is bounded as a whole: breadth blowup at bounded depth is an
  `Expand_error` `BudgetExceeded` naming the innermost macro and, for a syntax
  operator, its site - the application installs its error on the budget, so
  the overrun is raised with its site and nothing re-catches it. The driver's
  own elaboration requests report overruns through `Elab_entry.reporting_budget`
  like every other checker request
  ([expansion-errors-reach-the-user-raw](wayfinder/tickets/expansion-errors-reach-the-user-raw.md)). A type-aware call's request also
  covers elaborating its output.
- **Expansion failures are error values** (M8): kind mismatch, non-syntax
  result, non-declaration result, self-expansion during definition and a
  missing callback are `Expand_error.Error { error; site }`, with the syntax
  operator's use and declaration spans as the site. No `failwith` remains in
  `expand.ml`, and the catch-all that re-wrapped a macro body's exceptions as
  strings is gone
  ([macro-fuel-is-the-evaluation-budget](wayfinder/tickets/macro-fuel-is-the-evaluation-budget.md)).
  Any evaluation failure inside a macro application (`panic`, division by zero,
  a runtime match failure) is `Expand_error.EvalFailed` with the application's
  site: the evaluator fails through one helper, `Nbe_support.fail`, which raises
  the error of the macro application running under the budget, or `EvalError`
  outside one. Primitive reducers return a failure instead of raising
  ([macro-body-eval-errors-lack-site](wayfinder/tickets/macro-body-eval-errors-lack-site.md)).

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
  by spelling among locals. A path's head (qualified pattern heads, record-pattern
  types, `perform E.op`, effect branches, `impl M.Trait`) resolves the same way,
  and traits and nominal types are located through the entry the head resolves
  to - by trait identity, or by applying a type former - not by a name-keyed
  table or an environment scan. `impl M.Trait(..)` and `[A : M.Trait]` now work
  ([names-resolve-without-spelling](wayfinder/tickets/names-resolve-without-spelling.md)).
  Still by spelling: a constructor label matched inside a scrutinee's known nominal.
- **Macro type binders are explicit.** `macro m[A](x) : Expr(A)` binds `A` (a
  reflected type, `Syntax.R`, unless annotated), so arity is syntactic. (Superseded
  2026-09-15 by macro signatures, above: any number of binders, `T` checked.) The uppercase rule, the
  `Macro_resolver` pass, the parse-time adapter and `: A` binders are deleted
  ([macro-type-binders-should-be-explicit](wayfinder/tickets/macro-type-binders-should-be-explicit.md)).
  `Elab_infer` no longer special-cases `EffectRow` or `stx_` names
  ([elaborator-matches-names-by-spelling](wayfinder/tickets/elaborator-matches-names-by-spelling.md)).
- **Macro bodies** elaborate inside the unit opens around their definition,
  nothing ambient (M3). Units that write macros open the prelude themselves.
- **Types.** `type A = … and B = …` chains are mutually recursive nominals. Nested
  patterns through recursive positions work (they read constructors by nominal id).
- **Syntactic roles (M7).** Raw tokens carry scope sets and every id takes its
  token's. A syntax form or operator resolves by scope set (largest subset,
  ambiguity loud); units, modules, structs and blocks scope their tokens, so
  syntax shadows syntax lexically and a template's replacement sees roles as of
  its definition. Template intro scopes go on replacement tokens: syntax a
  template names itself is invisible to user code, and a hole may name a
  generated declaration (`syntax $n { $n $x => … }`, `infix ($op) …`). A role
  never mixes with another binder of its name: syntax declarations survive as
  `SyntaxBinding` / `SyntaxDef`, imported roles are seeded into the expander,
  and the binder funnel raises `RoleConflict` in either order (application-written
  binders and a fixity attached to its value excepted); an open supplying a
  visible role's name is `OpenSuppliesRole`
  ([template-heads-resolve-by-scope-set](wayfinder/tickets/template-heads-resolve-by-scope-set.md)).
  Enforestation still precedes expansion; the expander-driven loop rides on M9.
- M9 (templates desugar to macros) is complete; see "Macro model M9, run 3".
  The macro model's remaining distances are in the design map.

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
  macro expansion guarded by the evaluation budget, shared across copied contexts),
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
