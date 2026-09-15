---
title: A syntax form's capture extent is chosen by catching parse errors
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
assignee:
blocked_by:
---

# A syntax form's capture extent is chosen by catching parse errors

## Defect

`Enforest_template.try_prefixes` picks how many tokens an `Expr` / `Pattern`
hole captures by trying ever-longer prefixes and treating
`Error _ | Unsupported _` from the parser as "not this one". Exceptions as
control flow, which `CLAUDE.md` forbids; carried over from the pre-M9 matcher.
It is also quadratic in the capture length (each prefix re-parses from the
start and `prefix @ [term]` copies).

## Direction

The parser entry points the matcher calls (`parse_expr`, `parse_pat`) return a
`result` (or an option-returning variant used only here), so a failed prefix is
a value. Better: let the parser report how far a well-formed expression extends
("parse the longest expression, return it and the rest"), so the matcher does
one parse per hole instead of one per prefix. Check the "hole ending a group
takes the whole group" behaviour (brace syntax, dc4594f) survives.

## Resolution (2026-09-15)

A capture reads as far as its parser does: an `Expr` / `Pattern` hole calls a
parser that returns what it read and the rest (`parse_expr_prec`,
`Enforest_pat.parse_pat_prefix`), one parse per hole, no exception caught. A
`Decl` hole, captured unread, extends to the pattern's next literal or the end.
`try_prefixes` is deleted.

Decisions (user, 2026-09-15):
1. **The hole ending a use reads at the form's role precedence**, like a
   prefix operator's operand: `syntax inc { | inc $x => $x + 1 }` (prefix 50),
   `inc 1 * 10` is `(inc 1) * 10`.
2. **Literals: the parser decides.** A literal inside a capture is read by it:
   `when True if (False) { 1 } else { 2 } else 0` is 2.
3. **Whitespace never ends a capture.** A spaced `(…)`/`[…]`/`{…}` after an
   expression is the usual "must be adjacent" error, inside a capture too. A
   hole ends at `;`, a rule literal nothing else consumed, or its precedence.
   A rule needing a condition before a group parenthesises it:
   `syntax while { | while ($c) $(b : Block) => … }`.
4. **A `Pattern` hole reads one alternative**; `(A | B)` needs parentheses. A
   pattern's juxtaposed arguments follow only a bare constructor (not a group,
   not one with a `(…)` argument list).
5. **A `Decl` hole** extends to the pattern's next literal, else to the end.

Implementation choice on 1, pending confirmation: a hole the pattern bounds -
followed by a literal or hole, or inside a group - reads a whole expression
(precedence 0). Uniform role precedence breaks
`choose flag yes then 40 + 2 else 0` (`$branch` reads `40`, `+` is 10 < 50).
A syntax form's precedence is fixed at 50 today; no surface declares a lower
one.
