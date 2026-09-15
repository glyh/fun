---
title: Brackets decide grouping — structural hole extents, Rust-style arms
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
decided: 2026-09-15 (partly — see Open)
assignee:
blocked_by:
---

# Brackets decide grouping — structural hole extents, Rust-style arms

## Why

The capture-extents fix (branch `capture-extents`, unmerged) surfaced a cluster
of "how far does a hole read?" questions: a trailing hole (`inc 1 * 10`), a hole
bounded by a keyword (`then 40 + 2 else`), a condition before a group
(`while x < n { … }`), whose `else` a capture holds, and `|` being both the arm
separator and pattern union. The branch answers them with a split parsing
policy. Rhombus (shrubbery notation, `/home/lyh/pullground/rhombus`) answers most
of them structurally instead: the reader's tree decides extents before any macro
runs. We adopt that idea **without** layout: `fun` has no indentation-sensitive
syntax, so `{}`, `[]`, `()`, `,` and `;` carry the structure.

## Decision

1. **Brackets decide grouping.** `{ … }`, `[ … ]` and `( … )` group; `,` and `;`
   separate. An extent is never chosen by a parser guess.
   - A hole is bounded structurally: a whole group, or the trailing hole.
   - A bare keyword does not end an expression hole. Branches are brace groups:
     ```fun
     syntax choose { choose ($c) $(a : Block) else $(b : Block) => … }
     choose (flag) { 40 + 2 } else { 0 }
     ```
   - A condition before a group is parenthesised: `while (x < n) { … }`.
   - Nesting is owned by braces: `when (True) { if (False) { 1 } else { 2 } } else { 0 }`.
2. **Arms are Rust-style; `|` is only union.** Match arms, effect branches and
   syntax-form rules drop the leading `|`:
   ```fun
   match (n) {
     A | B => { x = 1; x + 1 }   // brace body: comma optional
     Some(x) => x + 1,           // expression body: comma required
     _ => 0                      // last arm: comma optional
   }
   ```
   - A body that is exactly one `{ … }` group ends its arm: `A => { 1 } + 2` is
     an error, write `A => ({ 1 } + 2)`.
   - Unlike Rust, `if`/`match` bodies are not exempt: `A => if (c) { 1 } else { 2 },`
     needs its comma. Rule in one line: **brackets end an arm; otherwise `,` does.**
   - `|` means union everywhere: `type Option = None | Some(A)` and the pattern
     `None | Some(_)` read alike. An effect row's tail `{E | r}` sits inside its
     own braces and is unaffected.

## Open (grill before implementing)

- **Precedence on syntax forms** (Rhombus: `~weaker_than`/`~stronger_than`,
  relative, with "needs parentheses" when two operators have no declared
  relation — `enforest/main.rkt:372`). Today every form is prefix 50. Relative
  or numeric? Declared where (`prefix N name`, or in the `syntax` head)?
- **The trailing hole** reads at the form's precedence (`inc 1 * 10` =
  `(inc 1) * 10`) — decided 2026-09-15; whether a hole may also be written to
  parse "as if after operator `op`" (Rhombus `AfterPrefixParsed`) is open.
- **Tail-returning forms** (Rhombus `'macro` protocol: return expansion and
  unconsumed tail) as the escape hatch — wanted?
- **The `capture-extents` branch** (one-pass captures, `try_prefixes` deleted)
  currently implements a split rule for bounded holes; rebase it onto this
  decision rather than merge as is.
- **`m9-decl-params` branch** questions: a `Decl` argument arrives as one unread
  `DeclItems` (no per-item inspection without an `expand_block`-like reader); a
  `$d` hole accepts only a list.
