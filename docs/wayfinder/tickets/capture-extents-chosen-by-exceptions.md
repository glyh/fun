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
`try_prefixes` is deleted. Two parser rules make "as far as it reads"
well-defined: a group written apart from an expression ends it (the adjacency
error now comes from `parse_all` when such a group is left over), and a
pattern's juxtaposed arguments follow only a constructor that is not a group
and has no parenthesised argument list.

Behaviour changes (the old shortest-prefix choice was the artifact):
- a trailing hole takes the longest operand: `inc 1 * 10` is `inc (1 * 10)`,
  was `(inc 1) * 10`;
- a rule literal that is an infix operator in scope is read by the capture;
- an unparenthesised or-pattern `A | B` is no longer captured by a `Pattern`
  hole (write `(A | B)`);
- a `Decl` hole followed by another hole or group captures everything.
