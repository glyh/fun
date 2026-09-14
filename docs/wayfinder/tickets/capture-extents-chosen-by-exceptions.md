---
title: A syntax form's capture extent is chosen by catching parse errors
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
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
