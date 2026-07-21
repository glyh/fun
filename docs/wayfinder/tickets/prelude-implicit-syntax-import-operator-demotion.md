---
title: Prelude-as-implicit-syntax-import for operator demotion
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee:
blocked_by:
---

# Prelude-as-implicit-syntax-import for operator demotion

## Question

Build a mechanism so **prelude-declared operators reach user-code enforestation**,
then demote the arithmetic/comparison operator table (`+ - * / % == != < > <= >=`,
prefix `not`) out of `operator_env.ml`'s hardcoded `infix_table`/`prefix_table`
into prelude operator declarations.

## Context

- Graduated from [Stage 11 macro-powered language features spec](specify-stage-11-macro-powered-language-features.md);
  this is the "demote the operator table" thread.
- **Spike finding (blocker):** the enforester env for user code starts at
  `Operator_env.empty` (`enforest_util.ml:32`); builtin operators come only from
  the hardcoded table fallback in `find_infix`/`find_prefix`, and extra operators
  come only from **explicitly `import`ed files** via `load_syntax_exports`
  (`core_loader.ml`). The prelude (`stdlib_source`) is compiled into `init_ctx`
  and is **never** fed through the operator-collection path — so prelude-declared
  operators would silently vanish from user programs.
- The operators' *semantics* already live in the library (prims + stdlib `(==)`
  etc.); only the *fixity/precedence metadata* is hardwired. True demotion means
  letting the prelude declare that metadata and propagating it — essentially "the
  prelude is an implicit syntax import."
- Open sub-questions: where to seed the default `Operator_env.t` (or replace the
  fallback tables) with the prelude's exports; the bootstrap order (the prelude
  itself uses `==`/`!=`); and whether `<-` (genuine ref machinery) stays builtin.

## Update — a stepping-stone landed (the `builtin_syntax_hook`)

Making `if` a stdlib syntax template (see
[Bool and `if` as library features](../topics/bool-and-if-as-library.md)) required
a first cut of this propagation. Current mechanism:

- `Enforest.builtin_syntax_hook : (unit -> Operator_env.export list) ref`
  (`lib/expand/enforest.ml`), default `[]`. `seed_builtin_syntax` applies whatever
  the hook returns to **every** `parse_expr`/`parse_module` env.
- `elab_prelude.ml` fills the hook with the stdlib's `pub syntax` exports (parsed
  from `stdlib_source` via `Enforest.parse_public_syntax_exports`, using a
  non-seeded env to avoid recursion). This inverts the layer dependency (the
  `expand` layer can't depend on `semantic`, so `semantic` registers into a hook
  the enforester exposes). `if` now lives in `stdlib_source`, not the enforester.

**Why it's only a stepping stone (the principled target):** the hook seeds prelude
syntax **globally and unconditionally** into every parse, not *because stdlib is in
scope*. It exists because stdlib isn't delivered as an importable module at parse
time — file imports carry their `pub syntax` through the `load_syntax` /
`parse_public_syntax_exports` channel, but the prelude is baked into `init_ctx` and
never goes through it. The principled version is to route the **implicit prelude
through that same import/`load_syntax` channel** so its syntax is scoped to stdlib
being in scope — then "no stdlib → no `if`" (and no operators) falls out naturally,
and the global mutable hook goes away. Same mechanism unlocks operator demotion.
In normal compilation the two are behaviorally identical (stdlib is always
implicitly opened); the hook only differs (wrongly) in edge cases like parsing a
fragment with no prelude.

**Deferred sub-question (revisit later):** whether the prelude should be
*implicitly* opened at all, or whether user code should have to open it explicitly.
This bears on the above (an explicit-open model makes syntax-scoping obvious) but is
a separate call — not decided here.

## Resolution

_Unresolved._
