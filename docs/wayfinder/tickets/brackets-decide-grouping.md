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
2. **Arms are Rust-style; `|` is only union.** (**Implemented** 2026-09-15, branch `rust-arms`: one structural splitter, `Enforest_util.split_match_branches`; prelude and tests migrated.) Match arms, effect branches and
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

3. **Precedence is relative, in named order groups** (grilled 2026-09-15; numbers
   replaced because "it is often unclear what precedence to assign at all").
   ```fun
   order comparison;
   order additive : stronger_than(comparison), assoc(left);
   order multiplicative : stronger_than(additive), assoc(left);

   infix additive + ;
   infix multiplicative * ;
   syntax inc : stronger_than(multiplicative) { inc $x => $x + 1 }
   ```
   - **Groups, not per-operator numbers.** An operator or syntax form joins a
     group, or declares its relation to groups.
   - **Transitive.** `a * b < c` = `(a * b) < c` without a direct declaration; a
     cycle is an error at the declaration.
   - **Associativity lives on the group** (default left).
   - **Undeclared relation is an error**, never a guess:
     `inc 1 ++ s` → "`inc` and `++` have no declared order; write
     `(inc 1) ++ s` or `inc (1 ++ s)`" (Rhombus, `enforest/main.rkt:372`).
   - **A form or operator with no group is weaker than every grouped one**: its
     trailing hole reads a whole expression, so `inc 1 * 10` = `inc (1 * 10)`
     (this revises the earlier same-day "form's precedence 50" answer).
   - **Groups are ordinary binders** (M7/M12): resolved by scope set, exported
     with `pub`, reached by `open` or `Std.additive`; no global table.
4. **Hole extents.**
   - **The trailing hole** reads at the form's order (above).
   - **A non-trailing hole matches exactly one term**: an atom or one bracket
     group (Rhombus `$x`). `choose (x < n) then (a + 1) else 0` is fine;
     `choose x < n then a else 0` is an error (`<` where `then` expected).

## Open

- **Tail-returning forms** (Rhombus `'macro` protocol: return expansion and
  unconsumed tail) — not now; ticket as an escape hatch when a case needs it.
- **Parse "as if after operator `op`"** (Rhombus `AfterPrefixParsed`) for a hole —
  not decided; no known case needs it.
- **Migration of numeric fixity** (`infix 10 +`, prelude operators) to groups.
- **The `capture-extents` branch** (one-pass captures, `try_prefixes` deleted)
  currently implements a split rule for bounded holes; rebase it onto this
  decision rather than merge as is.
- **`Decl` parameters** (merged, 09edfdb) questions: a `Decl` argument arrives as one unread
  `DeclItems` (no per-item inspection without an `expand_block`-like reader); a
  `$d` hole accepts only a list.
