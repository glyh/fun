---
title: A local open expression, M.(expr)
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
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
