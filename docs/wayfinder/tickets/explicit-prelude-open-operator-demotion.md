---
title: Explicit prelude open for operator demotion
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
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
   `load_syntax_exports` static path. **[not started]**
3. **Explicit prelude.** Reserved `"std"` → builtin prelude; `open (import "std")`;
   lift `Surface.Open`/`Syntax.Open` to accept an expression (core `Open` already
   takes an arbitrary term); delete implicit `open_stdlib` + fallback tables.
   **[partial]** — the two structural pieces landed (see progress note): `Open`
   now carries a module *expression*, and `import "std"` is a reserved path
   resolving to the builtin prelude module. The *delete implicit `open_stdlib` +
   fallback tables* part is deferred (it is coupled to steps 2/4).
4. **Demote operators.** Move `+ - * / % < > <= >= == !=` and prefix `not` into the
   prelude as `pub infix` / `pub prefix`. **[not started]**
5. **Tests to their layer.** `test/syntax/*` stay prelude-less (they test raw
   parsing); `eval_with_macros` / REPL / `macro_driver` adopt `open (import "std")`.
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

## Risks

- **Prelude bootstrap ordering.** The prelude self-parses prelude-less and must not
  forward-reference its own infix operators before their bindings advance. Today it
  mostly uses prim functions (`lt_i64`, `not(...)`) and prefix-paren `(==)` forms;
  step 4 must keep the prelude authored to respect advanced-order.
- **Deferred sub-question (revisit later):** whether the prelude should eventually
  become a genuine shipped `.fun` file imported by real path (zero special-casing,
  maximal C# uniformity) rather than a reserved builtin `"std"`. Not needed now.

## Resolution

_Unresolved (design decided; implementation pending, blocked on the table
unification child)._
