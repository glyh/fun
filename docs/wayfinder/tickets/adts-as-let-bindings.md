---
title: ADTs are declared by let bindings (`enum` expressions); `type` is deleted
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
decided: 2026-09-15
assignee:
blocked_by:
  - nominal-identity-applicative-by-purity.md
---

# ADTs are declared by let bindings (`enum` expressions); `type` is deleted

## Decision (grilled 2026-09-15)

Like records ([records-only-let-bindings](records-only-let-bindings.md)), a nominal
ADT is a value declared by a let binding. The `type` keyword goes.

```fun
Color = enum { Red, Green, Blue }
Option = fn(A : Type) { enum { Some(A), None } }
rec Tree = enum { Leaf, Node(Tree, Tree) }
rec Expr = enum { Lit(I64), Block(Stmt) }
and Stmt = enum { Do(Expr) }
```

**Spelling (grilled 2026-09-15):** `enum { A, B(T), … }` — constructors separated by
commas, like every other bracket group; `|` stays union.

## Consequences, and what each needs

1. **Same arguments give the same type.** `Option(I64)` must equal
   `Option(I64)`, so evaluating `enum { … }` under a function cannot mint a fresh
   type per call. Decided as E11 (nominal identity is applicative by purity),
   **not implemented**: today a nominal declared under a binder does not
   evaluate at all (`mk(I64)(1)` → `unbound nominal type`). Hence `blocked_by`.
2. **Constructors are members.** `Color.Red`, or `open Color; Red` — as struct
   members. `type` today puts `Red` in scope directly. The prelude opens its own
   (`Option`, `List`, …).
3. **Constructors of a type function.** `Option.Some(1)` applies `Option` to a
   fresh `A` and takes `Some` — the rule already decided for pattern heads
   ([pattern-head-accepts-type-formers](pattern-head-accepts-type-formers.md):
   aliases work), now in expressions too.
4. **Mutual recursion is `rec … and …`.** The `type … and …` three-phase knot
   moves onto the let group shared with recursive records, so an ADT and a
   record may sit in one group.
5. **Indexed families are not expressible** (`Vec(n)` with `Nil : Vec(0)`,
   `Cons : Vec(n) -> Vec(n + 1)`): `fn(n) { enum { … } }` cannot give a
   constructor its own index. Nothing uses them today; a later `enum` extension
   would be needed.

## Work

- After E11: `enum` expression, constructor members, `Option.Some` through a
  former, `rec … and …` knot, delete `TypeBinding`/`TypeDef` and the `and` chain.
- Migrate the 15 prelude types, compiler-known `Expr` ADTs and their pattern
  synonyms (see `CLAUDE.md`, "Adding a new Syntax ADT"), and every test.

## `type` as sugar — decided: a prelude macro (2026-09-15)

The user may keep `type … = …` for ADTs **only as sugar**: a let binding of the
`enum` plus an open of its constructors, so the branches are in scope without
writing `open`.

```fun
type Color = Red | Green | Blue
// sugar for:
Color = enum { Red, Green, Blue }; open Color
```

**Decided:** `type` is not compiler syntax. It is a `: Decl` macro in the prelude
(Stage 11, library-level features) expanding to exactly the bind and the open:

```fun
type Option A = Some(A) | None
// expands to:
Option = fn(A : Type) { enum { Some(A), None } }; open Option
```

- `type` stops being a keyword; it is a prelude role like any syntax form, so a
  user can shadow or not open it.
- The macro reads its alternatives from tokens (`|`-separated constructors, an
  optional parameter list) and builds the `enum` with quoted syntax; no compiler
  support beyond `enum`, M9 parameter kinds and `quote { … }`.
- Only for ADTs: records have no `type` form.

## Grilled (2026-09-16): macro-system fixes first; enums before the macro

The adts-as-lets run found the `type` macro cannot be written today: syntax forms
are not tried after `pub`; `and` chains and variable parameter lists do not fit
hole rules; `type` lexes as a keyword. Decided:

- **Fix the macro system generally** (not built-in sugar, not tail-returning
  macros): (1) syntax forms (and procedural macro calls) apply after `pub`;
  (2) a hole kind that captures the rest of the item as unread tokens, which the
  macro reads itself; (3) `type` becomes an ordinary identifier.
- **Order:** step 1 — `enum { … }`, constructor members, `Option.Some` via a
  former, `rec … and …` for enums, with `type` still compiler syntax; step 2 —
  the macro-system fixes; step 3 — `type` as a prelude macro, migrate the ~350
  test declarations and 15 prelude types, delete `TypeBinding`/`TypeDef`.
- **Constructor shadowing:** constructors reach scope through `open`, so a later
  same-named binding shadows them like any opened name. Accepted.
- Type parameters become E11 captures (`Option = fn(A : Type) { enum … }`),
  reaching `VNominal.params`, `build_ctor`, refinement, unification, quoting and
  reflection — expect several green steps.
