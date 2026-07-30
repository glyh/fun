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
   install `<-` as a base binding; delete `Operator_env.t`.
2. **Driver-carries-operators.** Route `open`/import operator delivery through
   `Macro_driver` alongside macros; delete `builtin_syntax_hook` and the
   `load_syntax_exports` static path.
3. **Explicit prelude.** Reserved `"std"` → builtin prelude; `open (import "std")`;
   lift `Surface.Open`/`Syntax.Open` to accept an expression (core `Open` already
   takes an arbitrary term); delete implicit `open_stdlib` + fallback tables.
4. **Demote operators.** Move `+ - * / % < > <= >= == !=` and prefix `not` into the
   prelude as `pub infix` / `pub prefix`.
5. **Tests to their layer.** `test/syntax/*` stay prelude-less (they test raw
   parsing); `eval_with_macros` / REPL / `macro_driver` adopt `open (import "std")`.

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
