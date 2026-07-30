---
title: Add short-circuit && / || operators
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
assignee: glyh
resolution: Implemented as stdlib `pub infix` syntax templates expanding to `match` over Bool (not builtin operator_env entries), with a lexer simplification — `&`/`|` folded into operator_chars (so `&&`/`||` lex by maximal munch), redundant per-operator rules and the dead At token removed. Structural punctuation (`|` Bar, `->`, `=`) keeps dedicated tokens.
closed_date: 2026-07-30
blocked_by:
---

# Add short-circuit && / || operators

## Question

Add `&&` and `||` as short-circuit boolean operators over the library `Bool` ADT.

## Context

- Graduated from [Stage 11 macro-powered language features spec](specify-stage-11-macro-powered-language-features.md).
- They must be **macros/templates, not functions** — the RHS must not be evaluated
  eagerly. Expand `a && b` ⇒ `match a do True -> b | False -> False end` and
  `a || b` ⇒ `match a do True -> True | False -> b end`.
- Feasible **now** as builtin operators (same status as `<-`): add entries to
  `operator_env.ml`'s `infix_table` with a `Template`/`Macro` expansion, and add a
  lexer path for `&`/`|` (`raw_syntax.ml:92` — `|` is currently a bare `Bar`).
- Does **not** depend on the prelude-operator-propagation mechanism.

## Resolution

Implemented — as **stdlib syntax templates**, not builtin `operator_env.ml`
entries (better than the sketch above: nothing hardcoded in the compiler):

- `elab_prelude.ml` `stdlib_source` gains
  `pub infix (&&) 4 Left ($a, $b) -> match $a do True -> $b | False -> False end`
  and `pub infix (||) 3 Left ($a, $b) -> match $a do True -> True | False -> $b end`,
  seeded into every parse through the existing builtin-syntax hook (same
  mechanism as `if`). Template expansion defers `$b`, so both short-circuit.
  Precedence: comparisons (5) > `&&` (4) > `||` (3) > `<-` (1).
- Lexer simplified rather than special-cased: `&`/`|` folded into
  `operator_chars`, so `&&`/`||` (and future ops like `|>`) lex by maximal
  munch with no per-operator rules; the redundant explicit rules for
  `<-`/`==`/`!=`/`>=`/`<=` and the dead `At` token deleted. The design line
  settled on: **operator space lexes uniformly as `Operator`, but structural
  punctuation keeps dedicated tokens** — a lone `|` is `Bar` (separator for
  match branches / or-patterns / sum types / effect rows, not an operator),
  same category as `->`/`=`.
- Regression tests in `test_core.ml` eval suite: truth tables, both
  short-circuit directions (`panic` on the unevaluated side), `&&`-over-`||`
  and comparison-over-`&&` precedence, use inside `if`.
