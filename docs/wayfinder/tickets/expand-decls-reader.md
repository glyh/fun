---
title: `expand_decls` — read a Decl argument's items, parsed in order
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
decided: 2026-09-15
assignee:
blocked_by:
---

# `expand_decls` — read a Decl argument's items, parsed in order

## Why

A `(d : List(Decl))` macro parameter arrives unread (one `DeclItems`), because a
declaration can change how the next one parses (M9: bodies stay raw until
expansion reaches them):

```fun
twice_decls({
  syntax inc { inc $x => $x + 1 };
  y = inc 1          // parses only after `inc` is registered
})
```

A macro can read its tokens (`Syntax.tokens`) but has no way to get the parsed
declarations. `Block` parameters have `expand_block`; `List(Decl)` has nothing.

## Decision (grilled 2026-09-15)

Add `expand_decls(d) : List(Decl)`, the `List(Decl)` counterpart of
`expand_block`: it expands the items form by form in the macro's definition
context rules (roles bound by earlier items are visible to later ones), spends
from the running application's budget, and returns the expanded declarations.
The result may be placed back into output; expansion is idempotent (M9).

Tests: count the items of a group; a group whose later item uses syntax an
earlier item declares; placing `expand_decls(d)` back into `quote { … }`;
budget exhaustion inside it names the macro.
