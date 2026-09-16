---
title: Small follow-ups from the 2026-09-15 runs
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-16
resolution: All items done. Item 2 (a method row naming a parameter) fixed on branch small-followups-2; items 4, 6, 7, 8, 10 verified already done. The run found a separate pre-existing defect, ticketed as method-signature-metas-capture-self.
assignee:
blocked_by:
---

# Small follow-ups from the 2026-09-15 runs

1. **`self.a(2)` inside a struct fails at run time** with "field not found"
   (pre-existing; `a(self)(2)` works). Found by method-rows.
2. **Done 2026-09-16.** **A method's row is expanded in the struct's scope**, not the
   parameters' scope, so it cannot name a parameter. Found by method-rows.
3. **A syntax form's `$(d : Decl)` hole must match the parameter kind** (M9: a
   capture `$(x : T)` is the parameter `(x : T)`): `Decl` captures exactly one
   declaration in a `{ … }` group, `List(Decl)` a group of any number. Today the
   hole kind `Decl` still reads the group as a list (decl-param-one left it).
4. **Done 2026-09-16 (bounds are the set `{Eq, Show}`).** **`Eq + Show` trait-bound sugar is recognised by the written name `+`**
   (`elab_syntax_util.trait_bound_forms`) — an M12 survivor.
5. **Return-type annotations `fn(n : I64) : T { … }` fail** (on main; found by the
   tuple-types run). **Grilled 2026-09-15: support them** — `:` after the
   parameter list means the result type, as `x : T = …` does for bindings; the
   body is checked against it. Applies to `fn`, `method` and macro bodies alike.
6. **`Tuple(0 - 1)` escapes the checker as `EvalError`**, as a `panic` in a type
   does: evaluation errors during checking should be elaboration errors.
7. **Done 2026-09-15.** **Refs: error text shows the heap, not the ref** — `Mutate(?130)` instead of
   `Mutate(r)` (refs-effect-rows run).
8. **Done 2026-09-15 (a `let`/block discharges too).** **Refs: discharge only at function boundaries and the entry**, not at any
   `let` / block whose result and captures don't mention the heap as grilled.
9. **Done 2026-09-15 (one pass, `Elab_effects.local_heaps`).** **Refs: the alias check scans every older meta** per candidate heap
   (`ponytail:` marked) — quadratic.
10. **Done 2026-09-15 (`EffectRef` carries the id).** **`EffectRef` finds its family by name**, so a user `effect Mutate` could
    shadow the built-in — M12 survivor; resolve the built-in family by identity.

## Done (2026-09-15, branch small-followups)

- **1.** `self.a(2)` and `v.a(2)` are method-call syntax (user decision
  2026-09-15). Root cause of the old run-time "field not found": a record value
  holds only its fields, and `Dot` on it never looked at its type's methods;
  the checker let it through because `self`'s partial type turned any name into a
  new field constraint. Now `v.m` on a record with no field `m` evaluates to its
  type's method applied to `v`, and checks as that call. Inside a method every
  method's type is known before any body (annotations, else metas the body
  solves), so `self.later(…)` works; an unknown name on `self` is an error. A
  method declared `m()` is called `v.m()`, as `fn()`. Struct types with methods
  still differ from their fields alone.
- **3.** `$(d : Decl)` takes exactly one declaration, `$(d : List(Decl))` any
  number (a brace group or the items up to the hole's extent), as the parameter
  kinds; `Decl` names one declaration everywhere (`Syntax.hole_kind_of_name`).
- **5.** `fn(x) : T { … }` and `method m() : T can {E} { … }` check the body
  against `T` (order as an arrow type `A -> T can {E}`). The result type ends at
  the first top-level `{ … }` or `can`, so a type holding braces is parenthesised.
  Macro `: Expr(T)` / `: Decl` annotations are unchanged.
- **6.** An evaluation error while checking a form (`Tuple(0 - 1)`, `panic` in a
  type) is `ElabError (EvaluationFailed { message; site })`, converted at the one
  place every form passes through (`Elab_driver.at`).

### Item 4 grilled (2026-09-16, revises 2026-09-15): bounds are a set, `{Eq, Show}` — **done** (branch effect-arrows)

A binder's trait bounds are written as a set with the same brace-and-comma
notation as effect rows: `fn[T : {Eq, Show}](a : T, b : T)`; a single bound may
stay bare (`[T : Eq]`). Order is irrelevant; a duplicate is an error. `+` is not
used for bounds and stays arithmetic. Verified 2026-09-16:
`Elab_syntax_util.trait_bound_forms` reads `Syntax.TraitBoundSet` and never the
spelling `+`; the earlier "`+` as a trait method language-wide" question is moot.

11. **Match on a list inside a recursive function fails at run time**:
    `Cons(m, Nil) => …, Cons(m, rest) => f(rest)` → "match on non-constructor
    value" (found by the staged-prelude run). **Fixed 2026-09-16:** a decision
    tree leaf bound variables in the order its columns were resolved, not source
    order, so a nested refutable sub-pattern (`Nil` in `Cons(m, Nil)`) swapped
    `m` and `rest` and the call recursed on the head. Leaf bindings are now sorted
    into source order (`Core_match_compile.collect_leaf_bindings`), and the
    elaborator's binder lists for nested multi-binder sub-patterns are in source
    order too (they were reversed within each sub-pattern).

## Done (2026-09-16, branch small-followups-2)

- **2.** A method's row is expanded inside its parameters' scopes
  (`Expand.expand_method_parts`), as a dependent arrow's row already was, so
  `pub method bump(r : Ref(I64)) ->{Mutate(r)} I64 { … }` resolves `r`. The
  elaborator already read the row with every parameter bound.
- **Verified already done:** 4 (bounds are `TraitBoundSet`, no `+` spelling),
  6 (`Tuple(0 - 1)` and `panic` in a type are `ElabError (EvaluationFailed …)`),
  7 (`UnhandledEffects "effect Mutate(r)"` names the ref), 8 (a `let`/block with
  private mutation is pure), 10 (a user `effect Mutate` does not shadow the
  built-in).

Every item on this ticket is now closed; the run's remaining find is spun out as
[method-signature-metas-capture-self](method-signature-metas-capture-self.md).
