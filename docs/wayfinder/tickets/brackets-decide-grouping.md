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
   (**Implemented** 2026-09-15, branch `order-groups`; spelling as below.)
   ```fun
   order comparison;
   order additive : stronger_than(comparison) assoc(left);
   order multiplicative : stronger_than(additive);

   infix (+) additive;
   infix (*) multiplicative;
   order incs : stronger_than(multiplicative);
   syntax inc incs { inc $x => $x + 1 }
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
4. **Hole extents.** (**Implemented** 2026-09-15, branch `order-groups`.)
   - **The trailing hole** reads at the form's order (above).
   - **A non-trailing hole matches exactly one term**: an atom or one bracket
     group (Rhombus `$x`). `choose (x < n) then (a + 1) else 0` is fine;
     `choose x < n then a else 0` is an error (`<` where `then` expected).

## Open

- **Tail-returning forms** (Rhombus `'macro` protocol: return expansion and
  unconsumed tail) — not now; ticket as an escape hatch when a case needs it.
- **Parse "as if after operator `op`"** (Rhombus `AfterPrefixParsed`) for a hole —
  not decided; no known case needs it.

## Implementation notes (branch `order-groups`, 2026-09-15)

- **Spelling.** `order g : stronger_than(a, b) weaker_than(c) assoc(right)` -
  clauses follow each other, since a `,` ends a statement. An operator or form
  *joins* a group by naming it after its name: `infix (op) g`, `prefix (op) g`,
  `syntax name [: Decl] g { … }`; relations are declared only on groups. A number
  there is an error naming the new form. `pub order` exports a group.
- **Data.** `Syntax.role` carries `order : order option` (the numeric
  `precedence` and per-operator `assoc` are gone); an `order` carries its unique
  identity and the orders its declaration names, so two roles compare wherever
  they travel (`Syntax.order_relation`, the transitive closure). A group is a role
  binder with meaning `OrderGroup`, resolved by scope set; it never conflicts with
  a value or another role of its name. Reflected as `Syntax.Order`.
- **Prelude.** `disjunction < conjunction < comparison < additive <
  multiplicative < negation`, reproducing the old numbers.
- **`<-` joins no group**, so `a <- b <- c` is now a "no declared order" error
  (it was right-associative); `r <- n + 1` is unchanged.
- **The built-in grammar** keeps two fixed positions: after `->` and a `Tight`
  argument (`perform`, `ref`, `resume`, `can`), which no infix operator continues.
- **Holes.** A hole followed by `,` or `;` reads to it (it ends its item). A
  non-trailing `Decl` hole is one brace group of items (the pattern-literal rule
  is gone).
- **Forms rewritten** (the language rule, uses parenthesised):
  `choose (flag yes) then (40 + 2) else 0`, `pick (bool yes) then (add2 5) otherwise …`,
  `(twice 1) + match …` (an ungrouped form's trailing hole now reads the `+`).
- **`Decl` parameters** (merged, 09edfdb) questions: a `Decl` argument arrives as one unread
  `DeclItems` (no per-item inspection without an `expand_block`-like reader); a
  `$d` hole accepts only a list.

## Grilled after implementation (2026-09-15) — implemented (branch `order-group-leftovers`)

- `assoc(none)` is `Syntax.NonAssoc`, reflected as `NonAssoc`. `<-` and a
  compiler-known group `assignment` (`assoc(none)`) are base roles; the prelude
  declares `disjunction : stronger_than(assignment)`, so `assignment` is below
  every prelude group. An operator in a group unrelated to the prelude's still
  meets `<-` with "no declared order".
- A dotted group reference reads the group among the roles the unit the path's
  prefix denotes exports (`Expand.unit_path_of`, the same resolution macro
  members use), handed to the enforester as `unit_roles`. A path through an
  inline `module { … }` (not a unit) is "unknown order group".

- **`<-` gets its own group, weakest, and does not chain.** `r <- x + 1` is
  `r <- (x + 1)`; `a <- b <- c` is an error (it would store `Unit`). This needs a
  non-associative option on groups, `assoc(none)`. Not implemented yet.
- **A `Decl` hole that is not last takes one `{ … }` group** of declarations:
  `with_decls { x = 1; y = 2 } in x + y` (already what the order-groups run built).
- **`Std.additive`** (a group through a module path) is decided (groups are
  ordinary binders); only the parsing is missing.

## Found by the order-group leftovers run (2026-09-15)

- **`<-` is not weaker than a user group unrelated to the prelude's order.**
  `assignment` sits below `disjunction` only, so a group with no relation to the
  prelude groups cannot meet `<-` ("no declared order"). The grilled rule says
  `<-` is weaker than every group — decide how that is expressed without a
  special case (e.g. a group may declare `weaker_than(all)`, or `assignment` is
  the bottom every group is implicitly above).
- **`Std = import "std"` in the test helper fails with `OpenSuppliesRole "not"`**
  — likely pre-existing (an import binder of the prelude conflicting with the
  already-open prelude's roles). Investigate.

### Grilled (2026-09-15): `<-` is the default bottom, overridable

`assignment` (the `<-` group) is implicitly weaker than every group that does not
state its relation to it, so `r <- a <> b` works for any user group `mine`. A
group may override by declaring a relation to `assignment` explicitly
(`order mine : weaker_than(assignment)`); the explicit declaration wins. Express
it as a general property a group declaration can carry (e.g. `order assignment :
weakest assoc(none)`), not as a check for the name `<-`.
