---
title: Explicit prelude open for operator demotion
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
assignee: glyh
closed_date: 2026-07-30
resolution: Implemented — operators demoted to prelude pub infix/pub prefix (base_operators is just <-); builtin_syntax_hook ref and the blanket seed deleted; prelude delivered strictly via open (import "std") resolved through load_syntax on the reserved "std" path; Open carries a module expression; strict phase rule with REPL/entry points opening std by default. 792 tests green. Loose ends split into module-level-open-strict-imported-modules.md and retire-static-import-harvest-and-open-stdlib.md.
blocked_by:
  - unify-operators-into-scope-aware-binding-table.md
---

# Explicit prelude open for operator demotion

## Question

Make the prelude a module you `open` **explicitly**, delivered through the same
mechanism as every other module, then demote the arithmetic/comparison operator
table (`+ - * / % == != < > <= >=`, prefix `not`) out of the compiler and into the
prelude as ordinary `pub infix` / `pub prefix` declarations.

## Model (decided via grilling)

> **A module exports bindings. A binding may carry a compile-time meaning — a
> macro, or an operator's fixity. `open` brings a module's bindings (values +
> macros + operators) into scope through one hygienic table. The interleaving
> driver advances all of them uniformly. The prelude is just a module you `open`
> explicitly.**

Decisions locked:

1. **Reroute first, then demote** — build the clean delivery, *then* move the
   operators. (Demoting under the existing hook was rejected: it leaves the
   duplication and an OCaml-ism — the global mutable ref — that will not survive
   the C# rewrite.)
2. **Explicit prelude.** User code writes `open (import "std")`; `"std"` is a
   reserved path the loader resolves to the builtin prelude. No implicit
   `open_stdlib`, no auto-injection.
3. **`import` is an expression returning a module; `open` brings it into scope.**
   `open (import "..")` composes both facets. This is the single surface form.
4. **No parse-time magic — ride the existing interleaving.** Cross-module
   operators travel through the `Macro_driver` handshake that *already* carries
   macros across imports (Stage 8, `visit_macros`), **not** the static
   `load_syntax_exports` textual harvest. The only rule left is the one macros
   already obey: a compile-time export is visible only to code that comes *after*
   its (advanced) binding — you cannot use `+` before the `open` that brings it,
   just as you cannot call a macro before defining it.
5. **One table (see child ticket).** Fixity is an attribute on a `Binding` in the
   scope-aware `binding_table`; `Operator_env.t` is deleted; operators become
   hygienic. This is a precondition, split out as
   [Unify operators into the scope-aware binding table](unify-operators-into-scope-aware-binding-table.md).
6. **`<-` stays compiler-known** — installed as a base-context binding (fixity +
   `BuiltinRefSet` meaning), always in scope because it is core ref machinery, not
   stdlib. It does not demote into the prelude.

## Why the phase split forces the interleaving (the "magic" it removes)

Values are dynamic; syntax is static. `import` returning a module and `open`
bringing its fields into scope is an *elaborate/runtime* story. But
operators/templates must be known at **parse** time — you need `+`'s precedence to
finish parsing the rest of a file, before anything is evaluated. Today the
enforester papers over this by statically pattern-matching the literal
`import "stringliteral"` form and eagerly reading that file's `pub syntax`
(`load_imports_in_terms`) — one piece of text read two ways by two phases.

The interleaving driver dissolves that: when it *advances* the `open (import "..")`
binding, it elaborates the import to a module and harvests both its macros **and**
its operators from the resulting module, registering them before the next binding
is parsed. Macros already do exactly this; operators are the one compile-time
export still on the static path (`macro_driver.ml` line ~179 still calls
`Core_loader.load_syntax_exports`). Routing operators through the driver removes
the second mechanism.

## Context — what exists today (spike findings)

- The enforester env for user code starts at `Operator_env.empty`
  (`enforest_util.ml`); builtin operators come from the hardcoded
  `infix_table`/`prefix_table` fallback, prelude syntax (`if`, `&&`, `||`) comes
  from the global `Enforest.builtin_syntax_hook`, and imported-file syntax comes
  from `load_syntax_exports`. **Three delivery channels for one concept.**
- The **value** side is already the clean model, just implicit: `init_ctx`
  elaborates the prelude into a first-class `stdlib` module value; `on_expr` /
  `on_expr_effects` unconditionally call `open_stdlib` and wrap user code in
  `Open(stdlib, body)`. So every program today is elaborated as if
  `open stdlib in <code>`. Making it explicit means dropping that implicit open.
- Operators' *semantics* already live in the prelude (prims + stdlib `(==)`, `(<)`
  etc.); only the *fixity metadata* is hardwired. True demotion = let the prelude
  declare that metadata and deliver it through `open`.

## Layering note (why the hook existed)

`core_tt_expand` depends on only kernel + syntax; `core_tt_loader` sits above it;
`core_tt_typecheck` (home of the prelude string) sits above both. The expand-layer
parse chokepoint therefore cannot name the prelude upward — which is the *only*
reason the hook was an inversion `ref`. The one-table + driver model removes the
need: operator delivery happens at the driver/loader level (which *can* see
downward), not at a blanket expand-layer seed.

## Implementation sequence

1. **(child ticket)** Fixity into the binding table; precedence parser reads it;
   install `<-` as a base binding; delete `Operator_env.t`. **[DONE]**
2. **Driver-carries-operators.** Route `open`/import operator delivery through
   `Macro_driver` alongside macros; delete `builtin_syntax_hook` and the
   `load_syntax_exports` static path. **[done]** — the global mutable
   `builtin_syntax_hook` ref is deleted, and the blanket `?builtin_syntax` seed is
   gone (see progress notes 3–4). Operators are now delivered **only** where `std`
   is opened, resolved through `load_syntax` on the reserved `import "std"` path
   (in statement order via `Enforest_forms.parse_import`). The static
   `load_imports_in_terms` scan survives only for `syntax`-template bodies, not as
   the operator-delivery path.
3. **Explicit prelude.** Reserved `"std"` → builtin prelude; `open (import "std")`;
   lift `Surface.Open`/`Syntax.Open` to accept an expression (core `Open` already
   takes an arbitrary term); delete implicit `open_stdlib` + fallback tables.
   **[done]** — `Open` carries a module *expression*; `import "std"` is a reserved
   path resolving to the builtin prelude; the base fallback operator table is gone
   (only `<-` remains); the implicit `open_stdlib` for user code is deleted
   (`on_expr` elaborates as-is, the `open` is carried in the parsed term). The
   blanket `?builtin_syntax` DI is gone too (progress note part 4). `open_stdlib`
   survives *only* as a ctx-extension helper for macro compilation — split into
   [retire-static-import-harvest-and-open-stdlib](retire-static-import-harvest-and-open-stdlib.md).
4. **Demote operators.** Move `+ - * / % < > <= >= == !=` and prefix `not` into the
   prelude as `pub infix` / `pub prefix`. **[DONE]** — see progress note
   (2026-07-30, part 2). Done *before* the reroute (step 2), deviating from
   decision #1's "reroute first"; see the note for why that is safe here.
5. **Tests to their layer.** `test/syntax/*` stay prelude-less (they test raw
   parsing); `eval_with_macros` / REPL / `macro_driver` adopt `open (import "std")`.
   **[not needed as feared]** — `test/syntax/dune` links `core_tt_typecheck`, so
   `elab_prelude` fills `builtin_syntax_hook` at startup and the demoted operators
   reach those parses too. All 133 syntax tests stayed green with zero edits.
   **[not started]**

## Progress note (2026-07-30) — structural foundation landed

Steps 1–2 of the *structural* work are in; 789 tests green. What changed:

- **`open` takes an arbitrary module expression.** `Surface.Open` / `Syntax.Open`
  went from `(name, body)` to `(module_expr, body)` (core `Open` already took a
  term). `parse_open_statement` now parses the operand as an expression, so
  `open (import "std")` and `open <any module expr>` parse. Elaboration infers the
  module expr, checks it is a `VModule` (new `NotAModule` elab error replaces the
  old `UnboundVariable` hack), and opens it. All `Open` sites updated
  (expand add-scope/expand, lower/raise, template mapping, surface-rewrite,
  effect-collect).
- **`import "std"` is a reserved path → builtin prelude.** Resolved in the
  *typecheck* layer (`elab_infer`, returning the `stdlib` binding `init_ctx`
  already builds) so the loader never has to name the prelude upward across the
  layer boundary. The parse-time harvest paths that a loader triggers
  (`Core_loader.load_syntax_exports`, `Macro_driver.visit_macros`) short-circuit
  `"std"` to empty — the prelude's operators/`if`/`&&`/`||` are still delivered by
  `base_operators` + `builtin_syntax_hook` in this increment, so `"std"`
  contributes nothing extra yet and must not try to read a `std.fun` file.
  `Compiler_names.Module_name.std_import_path` is the single source of the name.

This is a deliberately *non-breaking, additive* stopping point: `open (import
"std")` works (redundantly with the still-implicit `open_stdlib` and the still-live
`base_operators`), so nothing is demoted or deleted yet. Regression tests:
`test/backend/test_core.ml` "open (import std) evaluates" / "… prelude value in
scope".

### Deferred, and the one open fork for step 3's remainder

The reroute+demote remainder (steps 2, 4, 5, and the deletions in step 3) is
coupled and was intentionally left for a follow-up. The **fork to decide first**:
the primary expression entry points (`eval_with_macros`, REPL `on_expr`) parse the
whole expression at once and rely on `base_operators`/`builtin_syntax_hook` seeding
fixity at *parse* time. Once operators are demoted to the prelude, parsing `1 + 2`
needs `+`'s fixity before elaboration runs — but explicit `open (import "std")`
only delivers it during elaboration. The interleaving driver resolves this at the
*declaration* level (advance the `open`, harvest operators, then parse the next
binding); a bare top-level *expression* has no such per-binding seam. So step 3's
remainder must choose how the expression entry points obtain prelude fixity —
recommended: **auto-advance a synthetic `open (import "std")` before parsing the
body** (keeps `eval_with_macros "1+2"` working, concentrates churn in the entry
points, not the ~700 test strings). This was surfaced but not yet decided.

## Progress note (2026-07-30, part 2) — operators demoted into the prelude

Step 4 is done; all suites green (339 core tests). Two commits:

1. **Bodyless operator-declaration grammar** (`enforest.ml`). The prelude needs
   *fixity-only* `BuiltinApply` operators (`a op b` → `op(a, b)` against the
   same-named value), but the grammar only had `Template`/`MacroOp` forms and *no*
   `prefix` form at all. Added `infix (op) prec assoc` (no body) → `BuiltinApply`
   infix, and a new `prefix (op) prec` → `BuiltinApply` prefix. Both emit an
   export-only `TemplateSyntaxDecl` (operator info, no runtime `Let`), so a `pub`
   one is harvested into a module's syntax exports like any other operator.
2. **Demotion** (`elab_prelude.ml` + `enforest_util.ml`). Added `pub infix
   (==) 5 Left … (%) 20 Left` and `pub prefix (not) 30` to `stdlib_source`;
   shrank `base_operators()` to just `<-`. The operators' *semantics* were already
   in the prelude (prims `+ - * / %`; functions `(<) (==) …`); only the fixity
   metadata moved out of the compiler.

**Why demote before reroute (deviating from decision #1).** Decision #1 said
"reroute first, then demote" to avoid *leaving duplication*. Demoting into the
prelude's single existing export channel (`stdlib_syntax_exports` →
`builtin_syntax_hook`) while *deleting* the parallel `base_operators` entries
*reduces* channels (2 → 1 for prelude operators), so it does not create the
duplication the decision guarded against. The remaining reroute (delete the global
`builtin_syntax_hook` ref + the static `load_syntax_exports` path, deliver via the
driver/`open`) is now the final cleanup and is unaffected by this order.

**No test churn.** The feared `test/syntax` breakage did not happen:
`test/syntax/dune` links `core_tt_typecheck`, so `elab_prelude`'s
`builtin_syntax_hook :=` runs at startup and the demoted operators reach raw-parse
tests too. All 133 syntax tests green untouched.

**What is left on this ticket** (the reroute, decision #1's mechanism):
delete `builtin_syntax_hook` (the OCaml global ref) and the static
`load_syntax_exports`/`load_imports_in_terms` harvest; deliver prelude + imported
operators through `open (import "…")` in statement order; resolve the entry-point
fork (auto-advance a synthetic `open (import "std")`) and delete implicit
`open_stdlib`. That is the C#-rewrite-survivability cleanup; the *user-visible*
goal (operators are no longer hardcoded in the compiler) is achieved.

## Progress note (2026-07-30, part 3) — the OCaml global ref is gone

The `builtin_syntax_hook` inversion ref — the specific "OCaml-ism that will not
survive the C# rewrite" that motivated this ticket — is **deleted**, replaced by
explicit dependency injection. 791 tests green.

- `Enforest.parse_expr` / `parse_module` (and the `Parse_expand` wrappers) gained a
  `?builtin_syntax` parameter; `seed_syntax` seeds it instead of reading the global
  ref. A parse with no stdlib passes nothing (prelude-less), so `if`/`+`/… are just
  identifiers — the raw-parse `test/syntax` suites now pass the exports explicitly.
- The prelude cannot be named from the expand/loader layers, so the exports are
  injected downward: entry points and the REPL pass
  `Lazy.force Elab_prelude.stdlib_syntax_exports`; `Core_loader.create` takes a
  `?builtin_syntax` and threads it into every module parse (so imported `.fun`
  files still see the operators) and returns it for the reserved `import "std"`.
- `Macro_driver.visit_macros` passes the loader's `builtin_syntax` when parsing
  imported modules for macros.
- `elab_prelude` no longer sets the ref; it just exposes `stdlib_syntax_exports`
  for callers to inject.

This is still a *blanket* per-parse seed (every parse that opts in gets the whole
prelude's operators), not yet the `open`-scoped, statement-ordered delivery. But
the layering inversion is dissolved: delivery is now ordinary downward DI that maps
cleanly to C#. **What remains**: make delivery `open (import "std")`-triggered in
statement order (retire the static `load_imports_in_terms` harvest) and drop the
implicit `open_stdlib` in favor of the explicit open — the last of step 2 + the
deletions in step 3.

## Progress note (2026-07-30, part 4) — strict phase rule; REPL opens std by default

The blanket `?builtin_syntax` seed is **removed**; the prelude is now delivered
strictly through `open (import "std")`. 792 tests green.

- **Enforest**: `parse_expr`/`parse_module` drop `?builtin_syntax` and gain
  `?open_prelude`. With `open_prelude:true` the parser harvests `std`'s exports
  (via `load_syntax` on the reserved path) *before* parsing and wraps an
  expression body in `Open (Import "std", body)`. Without it — and without an
  in-source `open (import "std")` (which `parse_import` harvests in statement
  order) — `+`/`if`/… are unbound identifiers. New regression test
  `strict phase rule: operators need std`.
- **Entry points open `std` by default**: the REPL and the program-evaluation
  test helpers pass `open_prelude:true`; loader-loaded modules likewise (there is
  no module-level `open` form yet). `Elab_prelude.std_load_syntax` answers the
  reserved path for parses with no loader.
- **`on_expr` no longer wraps** — the `open` is carried in the parsed term (from
  `open_prelude` or written by the program), so `on_expr` elaborates as-is. A
  program that opens nothing elaborates in the bare base context.
- **Macro transformer bodies** are compiler-facing (they use the `Syntax` API and
  operators), so they always elaborate with `std` open via the new
  `Elaborate.on_macro_body` (which wraps the body in `Open (Import "std", …)`);
  the macro-compilation callbacks route through it. This mirrors what
  `Macro_driver` already did with `open_stdlib`.
- **Raw-parse `test/syntax`** suites open the prelude explicitly (`open_prelude`
  + `std_load_syntax`) and peel the wrapper with a local `unwrap_std` so structural
  assertions are unchanged.

Ticket substance complete: operators are library declarations, delivery is the
single explicit-`open` path, no global mutable ref, and the phase rule is strict.
The remaining implicit `open_stdlib` survives only as a ctx-extension helper for
macro compilation. A genuinely module-level `open` form (so imported `.fun` files
are strict too, rather than auto-opened by the loader) is the one loose end, and
depends on adding module-level `open` support — out of scope here.

## Risks

- **Prelude bootstrap ordering.** The prelude self-parses prelude-less and must not
  forward-reference its own infix operators before their bindings advance. Today it
  mostly uses prim functions (`lt_i64`, `not(...)`) and prefix-paren `(==)` forms;
  step 4 must keep the prelude authored to respect advanced-order.
- **Deferred sub-question (revisit later):** whether the prelude should eventually
  become a genuine shipped `.fun` file imported by real path (zero special-casing,
  maximal C# uniformity) rather than a reserved builtin `"std"`. Not needed now.

## Resolution

**Resolved (implemented).** Operators (`+ - * / % == != < > <= >=`, prefix `not`)
are prelude `pub infix` / `pub prefix` declarations; `base_operators()` is just
`<-`. The global mutable `builtin_syntax_hook` ref and the blanket operator seed
are gone — prelude syntax is delivered strictly through `open (import "std")`
(resolved via `load_syntax` on the reserved `"std"` path, in statement order).
`Open` carries a module expression; the REPL and program-eval entry points open
`std` by default; the phase rule is strict (a program that opens nothing sees
`+`/`if`/… as unbound). 792 tests green. See progress notes parts 1–4.

Two loose ends were split into their own tickets rather than block closure:

- [Module-level open form (strict imported modules)](module-level-open-strict-imported-modules.md)
  — imported `.fun` modules are still auto-opened by the loader because there is no
  module-level `open` form yet; adding it lets modules be strict too.
- [Retire load_imports_in_terms and the open_stdlib survivor](retire-static-import-harvest-and-open-stdlib.md)
  — the static import scan and `open_stdlib` are off the delivery path but not yet
  deleted (both are still live for `syntax`-template bodies / macro compilation).
