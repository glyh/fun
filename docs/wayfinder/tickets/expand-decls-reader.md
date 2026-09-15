---
title: `expand_decls` — read a Decl argument's items, parsed in order
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-15
resolution: Implemented. `Syntax.expand_decls(d)` expands a Decl argument form by form in a copy of the running context and returns the expanded declarations; answered by the running macro application like `expand_block`, under its budget.
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

## Implemented (2026-09-15)

- `expand_decls` is a primitive answered by the running macro application
  (`Eval_budget.application.expand_decls`), wrapped as `Syntax.expand_decls :
  Decls -> Decls`. The items expand with `expand_struct_bindings_with_scopes` in
  a copy of the context, so what they bind stays inside the result.
- Both expand primitives wrap and unwrap with the running macro's own reflection
  types (`~nominals` on `Expand.run_application`), not the expander's.
- Both stay stuck on an open argument (a macro body being checked) instead of
  failing "runs only inside a macro application".

Found, not fixed (pre-existing on main): applying a let-bound lambda whose
parameter has a parameterised nominal type infers a wrong result type —
`{ f = fn(d : List(I64)) { d }; g = fn(d : List(I64)) { x : List(I64) = f(d); x }; 1 }`
fails with `NominalMismatch(List, List)` (the result prints as `List(Decl, I64)`).
The prelude wrapper is annotated (`expand_decls : Decls -> Decls = fn(d) …`) to
avoid it.
