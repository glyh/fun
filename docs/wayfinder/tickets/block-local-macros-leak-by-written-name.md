---
title: Block-local macros leak by written name
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
assignee: glyh
resolution: Instrumented `macro_head_key`. All four leaks, the nested `do` included, go through its one written-name fallback on ids that carry source scopes; there is no second path. Every legitimate fallback hit is on a context-less id (empty scope set), i.e. a string-built id or a binder-less registered macro. The fallback is now restricted to context-less ids, which is the S6 string fall-through that macros-have-no-quoted-syntax retires. Ids written in source resolve by scope set alone. Probes are a regression test, together with a positive in-block use. The table key is still the written name; that is harmless now that the fallback no longer reaches source ids.
closed_date: 2026-09-14
blocked_by:
---

# Block-local macros leak by written name

## Question

A macro defined inside a `struct`, `module` or nested `do` block is callable
after that block ends, whether or not it is `pub`. Values defined the same way
are not visible afterwards.

## Evidence

Found while researching
[struct-open-does-not-scope-over-con-fields](struct-open-does-not-scope-over-con-fields.md).
REPL probes:

```
do x = do macro mi(_) -> Syntax.i64(7); 0 end; mi(0) end          → 7
do M = module macro mi(_) -> Syntax.i64(7) end; mi(0) end         → 7
do R = struct macro mi(_) -> Syntax.i64(7) end; mi(0) end         → 7
do Q = struct macro mi(_) -> Syntax.var("I64"); g : I64 end
   R = struct f : mi(0) end; R{f = 1}.f end                       → 1   (sibling struct)
do R = struct g : I64; pub h = 2 end; h end                       → UnboundVariable "h"  (values don't leak)
do mi(0) end                                                      → UnboundVariable "mi" (control)
```

## Mechanism (binding-list path traced; `do` path not traced)

- `expand.ml`, `MacroBinding` case: the macro gets a binding-table entry with
  `resolved_name = binding_name`, the **written** name. It is registered in the
  expander's macro table under that same written name.
- `expand.ml`, `macro_head_key`: when scoped resolution returns `None`, as it
  should outside the block, the lookup falls back to
  `Expand_ctx.lookup_macro_entry ctx id.name`, which also uses the written name.
  That finds the leaked entry.

The fallback exists for imported and operator macros (per its doc comment).
Block-local macros share the table key space with them, so the fallback can't
tell them apart.

Not traced: the nested-`do` case (`MacroDef`) mints a fresh `resolved_name`,
yet still leaks, so a second path is involved, possibly the semantic driver's
pre-registration. Diagnose with instrumentation at `macro_head_key` before
fixing.

## Why it matters

- Hygiene: a later, unrelated `mi(…)` silently calls a macro from an inner
  block. Shadowing an intended outer or imported `mi` is order-dependent.
- Visibility: `pub` on macros means nothing inside a block.
- Port risk: the rule "block-local macros are keyed by written name" appears
  nowhere, so a port would copy it.

## Direction

Key block-local macros by a fresh `resolved_name`, as `MacroDef` intends, and
restrict the written-name fallback to entries that were actually imported or
declared as operators. Add the probes above as regression tests. The control
and the value case already pass.
