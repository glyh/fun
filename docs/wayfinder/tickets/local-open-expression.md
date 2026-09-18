---
title: A local open expression, M.(expr)
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: closed
assignee:
blocked_by:
---

# A local open expression, `M.(expr)`

Raised by the user (2026-09-17). Undecided: adopt it, and if so, what it means.

## Question

OCaml lets an expression open a module for itself only: `M.(e)` is `let open M in e`.
Should `fun` have the same form? Today the only way to open for one expression is a
block, `{ open M; e }`, which is a statement plus a body.

```
Color = enum { Red, Green };
f = fn(c) { Color.(match (c) { Red => 1, Green => 2 }) };
```

With the bare-constructor ruling (a raw enum's constructors are in scope bare only
after `open`), this is where the form would be used most.

## What it would touch

- **Grammar.** `e.(…)` is free today: `.` is followed by an integer (a projection) or a
  name (a member). Proposed: a postfix `.( … )` whose left side is a module
  expression, read with the same precedence as a member access. Open: does the left
  side have to be a *path* (`M`, `M.N`), as in OCaml, or any expression
  (`(f(x)).(e)`)? An arbitrary expression would be evaluated once, as `open` does.
- **Braces.** OCaml also has `M.{ … }` for records. Here `M.{a, b}` already means
  *export selection* (`export M.{a, b}`), and `P{x = 1}` is record construction; a
  `.{` form would collide. Probably parentheses only.
- **Meaning.** Exactly `{ open M; e }`: the open's region is `e`, so bare names inside
  may resolve to an open choice with this open as the innermost candidate
  (glossary **Open**, **Open choice**); roles and macros the module delivers bind
  inside `e` only (M7); an open's width is its type's public members (I2). No new
  core term: it expands to the existing `Open`.
- **Shadowing.** A name bound outside `M.(…)` and supplied by `M` resolves to `M`'s
  member inside (the open is inside the binder's region). Same as a block open; OCaml
  warns here (warning 44/45). Is a warning wanted, or is that out of scope
  (diagnostics are deferred)?
- **Patterns and types.** Does `M.(…)` also work in pattern position
  (`match (c) { Color.(Red) => … }`) and type position? OCaml allows it in patterns.
  Each is one more place the enforester reads it.
- **`OpenSuppliesRole`.** An open may not supply a member named like a role visible
  where it is written; that check applies unchanged.

## Not blocking

The port: the form is a surface addition that expands to an existing construct.
Decide before the prelude or library code starts relying on it.

## Resolution (2026-09-18) — not adopted

**Ruling: B, do not adopt `M.(e)`.** A block already opens a module for one
expression (`{ open M; e }`), and that is the whole of what the form would add: it
expands to the existing `Open` term and introduces no meaning a block does not
already have. A second spelling for one construct is the flexibility this language
trades away for consistency.

Cost accepted: the enum-match idiom stays double-braced —
`fn(c) { { open Color; match (c) { Red => 1, Green => 2 } } }` — in the prelude and
in library code. If that grows into a real irritant once stage 2's library code is
written against the bare-constructor rule, reopen this ticket with the call sites as
evidence; the grammar (`e.(…)`) stays free until then.

Closed without deciding the sub-questions it raised, which only exist if the form
does: path-only vs arbitrary left side, pattern and type position, and whether a
shadowing warning is wanted.
