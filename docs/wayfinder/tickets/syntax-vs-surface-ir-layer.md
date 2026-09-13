---
title: Syntax.t vs Surface.t — is one IR layer removable?
parent: ../fun-design-map.md
labels:
  - wayfinder:research
status: closed
assignee: glyh
blocked_by:
---

# Syntax.t vs Surface.t — is one IR layer removable?

## Question

Came out of the "too many IR layers" fog item. The pipeline keeps both `Syntax.t`
(what enforest and expand produce) and `Surface.t` (what the elaborator reads).
What does having both actually buy, and can one of them go?
Researched 2026-09-13.

Follow-up tickets:
[type-aware-macro-output-is-not-expanded](type-aware-macro-output-is-not-expanded.md),
[procedural-macros-capture-use-site-variables](procedural-macros-capture-use-site-variables.md),
[delete-surface-ir](delete-surface-ir.md).

## Findings

**1. The two trees have the same shape.** `lib/core_kernel/syntax.ml` and
`lib/syntax/surface.ml` have the same constructors, one for one, including every
`struct_binding`, `match_branch` and `pat` case. `lower_surface.ml` (127 lines)
does no desugaring. It only throws information away:

| Dropped by lowering | Where |
|---|---|
| `span` on every node | `Syntax.t = { kind; span }` → bare `Surface.t` |
| scope sets | `Syntax.id` → `string` (`lower_id id = id.name`) |
| `MacroDef.kind` | set to `None` on purpose |
| `SyntaxOperatorUse.unit` | discarded |

The reverse map, `surface_to_syntax.ml`, was deleted in `72caeef`. So this is now
a one-way erasure, not the round trip the fog entry described. `CLAUDE.md` and
some older docs still mention the deleted file.

**2. `Surface.t` does not mean "already expanded".** It still has `MacroCall`,
`MacroDef`, `SyntaxOperatorUse` and `StxExpr`. The elaborator either handles
these or `failwith`s on them (`elab_infer.ml`, "macro-only syntax should not
reach elaboration"). Having a separate type therefore guarantees nothing about
which phase a tree is in; the invariant is still only enforced at runtime.

**3. Scope sets are only partly used up before lowering.** `expand` gives
binders fresh names (`Lam`/`Let`/params via `extend_at_fresh`) and rewrites
`Var` uses to `resolved_name`. Type, constructor and effect names are bound with
`resolved_name = name.name`, so their written names pass through unchanged.
Dropping the scope sets is only safe to the extent that `expand` has already run
over the tree.

**4. Lowering is called in six places. Two of them skip `expand`.**
- `parse_expand.ml:9`, `macro_driver.ml:90,108` and `expand.ml:488,772` lower a
  tree that has already been expanded.
- `elab_infer.ml:951` and `elab_check.ml:166` handle type-aware macros
  (`: Expr(A)`). They lower the macro's **output** directly. Nothing inside it
  is expanded: nested macro calls, operators and binder renaming are all
  skipped.

**5. Elaborator errors carry no source location.** Once lowered, a tree has no
spans, and `Elab_error` has no position fields. Every type error is reported
without a location. This is a direct cost of the second IR.

**6. Code that is really written against `Surface.t`:**
`elab_surface_rewrite.ml` (197 lines, rewrites record self-references) and
`elab_effect_collect.ml` (278 lines). Across `elab_*` there are roughly 230
qualified `Surface.` references, plus unqualified matches. Tests match on
`Surface` in `test_macros.ml`, `test_expand_compat.ml` and one case in
`test_core.ml`.

## Defects found along the way

Reproduced by piping source into the REPL (`dune exec fun < probe`):

```
# A. Type-aware macro output is never expanded
do macro one(_) -> Syntax.i64(1)
   macro m(e) -> Syntax.ap(Syntax.var("one"), e)
   y : I64 = m(0); y end                               → 1
do macro one(_) -> Syntax.i64(1)
   macro m(e) : Expr(A) do do _ = A; Syntax.ap(Syntax.var("one"), e) end end
   y : I64 = m(0); y end                               → ElabError(UnboundVariable "one")

# B. Procedural macros capture use-site variables (templates do not)
do x = 1; macro m(e) -> Syntax.ap(Syntax.lam("x", e), Syntax.i64(2)); y : I64 = m(x); y end   → 2 (expected 1)
do x = 1; macro m(e) : Expr(A) do do _ = A; Syntax.ap(Syntax.lam("x", e), Syntax.i64(2)) end end; y : I64 = m(x); y end → 2
do x = 1; syntax li do | li $body -> do x = 2; $body end end; y : I64 = li x; y end          → 1
```

A is caused by finding 4. B happens on both macro paths, so its cause is not
lowering. The likely suspect is ids built with `Syntax.new_id` (scope `0`) not
receiving the macro-introduction scope. It is not diagnosed here. The existing
hygiene tests (`test_macro_hygiene_*`) only cover a binder that does not wrap
the argument, which is why they pass.

## Recommendation

**Delete `Surface.t` and have the elaborator consume expanded `Syntax.t`.**

- Going by findings 1 and 2, `Surface.t` is a copy of `Syntax.t` with less
  information in it. It doesn't mark a phase, so removing it gives up no
  guarantee.
- Gains: spans become available to elaborator errors (finding 5). The port
  carries one tree instead of two. The "add a field in N places" checklist in
  `CLAUDE.md` gets one entry shorter. The two unexpanded call sites (finding 4)
  now stand out: the fix for A is to run `expand` on macro output before
  elaborating it. Because that goes through the macro-runtime capability the
  elaborator already holds, it has to be done explicitly and is no longer
  hidden behind a type conversion.
- Cost: mechanical. Elaborator matches change from `Surface.Foo` to
  `{ kind = Foo; _ }`, and names from `string` to `id.name`. This touches
  roughly 475 lines in the two Surface-heavy modules plus the `elab_*` match
  sites. `lower_surface.ml` and `surface.ml` are deleted (~276 lines).
- Not recommended: turning `Surface.t` into a genuine post-expansion IR (no
  macro constructors, spans added). That is more work, and it pays off only if
  a phase-typed IR is wanted for its own sake. Nothing in the map asks for one.

Order: fix A first. It is small, and the collapse would otherwise change the
same call sites. B is a separate hygiene ticket. Then do the collapse. It does
not block the port, but the port should not copy both trees.
