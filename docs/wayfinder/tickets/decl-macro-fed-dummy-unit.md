---
title: An under-applied declaration macro is fed a dummy Unit
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-15
resolution: Every macro application (a call the reader knows, an application spine, an item-position call, a deferred typed call) checks its argument count against the parameters the macro declares before it runs, failing with ArgumentCount. The reader turns every call whose head names a macro into an exact-argument MacroCall, so `m(a, b)` is never a curried spine; `m()` passes the `()` an empty parameter list declares. Registration requires the parameter list. force_val's dummy syntax is deleted.
assignee:
blocked_by:
---

# An under-applied declaration macro is fed a dummy Unit

Found by the domain-model audit (2026-09-15), re-verified on `main` `fa2f32d`.

## Invariant

**M8**: kind checking is positional, and its error is an error. A macro never runs
on syntax it was not given.

## Where the code deviates

`lib/expand/expand.ml:1117-1122`: after applying a `: Decl` macro to its
arguments, `force_val` keeps applying any remaining lambda to a synthetic
`Atom Unit` expression until the result stops being a function. The
`ArgumentCount` check (`enforest.ml:27`) runs only when some parameter has a
non-`Expr` kind, so an all-`Expr` call is not counted.

## Example

```fun
macro two(a, b) : Decl { … };
two(x);   // runs with b = (); should be an ArgumentCount error
```

## Direction

Delete `force_val`. Check arity for every macro call, not only kinded ones, and
fail with `ArgumentCount`.

## Implemented (2026-09-15, branch `decl-macro-arity`)

- `Expand.check_argument_count` reads the declared parameters
  (`Expand_ctx.lookup_macro_params`, now required at `register_macro_kind`) and
  runs in `run_macro_call` and the `MacroCallBinding` case. An application
  spine takes exactly the declared count; a shorter spine is an error, extra
  arguments apply the macro's result (`ident(0)(5)`).
- `Enforest.macro_call_args` reads every call whose head names a macro as an
  exact-argument `MacroCall`, counted, not only kinded ones.
- **Left open:** a Decl macro's body has no declared output type, so a
  polymorphic result is left as an implicit function (`{ Nil }` is
  `{A} -> List(A)`). After every argument is given, such a result is
  instantiated with a type (`VU`), never with syntax. The root fix is a Decl
  output type the body is checked against — undecided between `Syntax.Decl` and
  `Syntax.Decls` (both are returned today: `Syntax.decl_let(…)` vs `quote { … }`).
