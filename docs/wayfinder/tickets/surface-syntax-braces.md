---
title: Surface syntax — brace bodies, `=>` arms, explicit semicolons
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-14
resolution: Implemented. Bodies are brace groups, `=>` separates patterns from results, `->` is only the function-type arrow, newlines are whitespace and a trailing `;` discards a block's value (block statements may be bare expressions). Removed forms fail naming the new form; the keyword-pair grouping helpers are deleted. The prelude and every test source were migrated by a throwaway rewriter over the old token structure; all tests keep their expected values.
decided: 2026-09-14 (grilled with M7)
assignee:
blocked_by:
---

# Surface syntax — brace bodies, `=>` arms, explicit semicolons

## Decision

Decided while grilling [M7](template-heads-resolve-by-scope-set.md): every
binder's region must be known to the reader, before any name means anything.

| Construct | Today | Decided |
|---|---|---|
| lambda | `fn(x) -> x + 1`, `fn(x) do … end` | `fn(x) { x + 1 }` |
| named fn / method | `fn eq(x, y) -> e`, `method get() -> e` | `fn eq(x, y) { e }`, `method get() { e }` |
| block | `do y = 1; y end` | `{ y = 1; y }` |
| if | `if c do t else e end` | `if (c) { t } else { e }` |
| match | `match v do A -> 1 \| B -> 2 end` | `match (v) { \| A => 1 \| B => 2 }` |
| effect branch | `effect E.op n -> e` | `\| effect E.op n => e` |
| template | `syntax x do \| pat -> repl end` | `syntax x { \| pat => repl }` |
| infix / macro body | `… ($a, $b) -> e`, `macro m(x) do … end` | `… ($a, $b) { e }`, `macro m(x) { … }` |
| module / sig / struct | `module … end`, `sig … end`, `struct … end` | `module { … }`, `sig { … }`, `struct { … }` |
| named module | `module M do … end` | dropped — `M = module { … }` |
| record type | `{x: I64}` | `struct { x: I64 }` |
| record construct / pattern | `Point{x = 1}` | unchanged |
| effect row | `can {IO \| r}` | unchanged |
| function type | `A -> B`, `(n : I64) -> Vec(n)` | unchanged |
| `multi … end` | | `multi { … }` |

- **`->` is only the function-type arrow; `=>` only separates a pattern from
  its result** (match arms, effect branches, template rules). `=>` becomes a
  reserved structural token like `=`, `|`, `->` (it already lexes as one
  operator token; no lexer rule needed). A pattern holds no bare `=>`; an arm
  body holds no bare `|`.
- **A bare `{…}` is always a block.** `if` and `match` heads are
  parenthesised, so `c { … }` never reads as a record construction on `c`
  (C/Zig).
- **An annotation ends at a bare `=`, `;`, `,`, `=>`, `|`, `{` or group end**,
  so a dependent binder's region (`n` in `(n : I64) -> Vec(n)`) is the rest of
  its annotation. Sound because every construct that could contain one of
  those is a group.
- **Newlines are whitespace; `;` is written.** A trailing `;` before `}`
  discards the block's value (Rust): `{ y * 2; }` is `Unit`.

## Notes

- Keyword-pair grouping (`do`/`sig`/`module`/`struct` … `end`) is reimplemented
  four times in `enforest_util.ml` today, disagreeing on openers
  (`collect_match_until_end` counts `match`/`trait`/`impl`; `multi` only
  sometimes). Braces delete all of it: the reader already groups `{}`.
- Migration touches the prelude (`elab_prelude.ml`) and every test source;
  `fn(` alone occurs ~208 times. Worth a mechanical rewriter over `Raw_syntax`
  rather than hand edits.
- Old `-> body` should fail with a targeted error naming the new form.
