# Macro use-case shortlist (idea store)

A curated shortlist of macro use cases that are *compelling specifically for
`fun`* — not the generic hygienic-macro fare. The point of collecting these is
that `fun`'s macro system sits on top of machinery a general-purpose macro system
does not have:

- **types-as-values** — a `Type` is an ordinary first-class value a macro can hold
  and inspect;
- **type-aware / type-providing macros** (Stage 10, `R = RExpr(Type)`) — a macro
  can both read the expected type at its use site and *compute* the type it
  produces;
- **[type-case](type-case-generic-programming.md) + [record-type reflection](record-type-reflection.md)** — structural walking of a
  type's shape at (runtime or macro) evaluation;
- **[traits as structural dictionaries](traits.md)** — an `impl` is just a record of
  evidence, so it can be *built* rather than only declared;
- **[algebraic effects](algebraic-effects.md)** — `perform`/handler code the macro can emit
  and validate.

Together these mean many things that are "compiler features" elsewhere can be
*library code* here. This doc is an idea store, not a frontier item — see the map's
[Open questions](../fun-design-map.md#open-questions) for what is actually scheduled.

> **Overall recommendation.** #2 (type-providing `format`) is the best *small,
> self-contained* demo — it needs nothing new. #1 (deriving) is the substantial
> follow-on that ties [Stage 11](../tickets/specify-stage-11-macro-powered-language-features.md)
> back to the [trait-deriving ticket](../tickets/design-trait-library-deriving-and-protocols.md).

---

## 1. Deriving as a library, not a compiler feature *(flagship)*

**Idea.** `derive` is an ordinary macro. Given a trait and a type, it walks the
type's structure via [type-case](type-case-generic-programming.md) +
[record-type reflection](record-type-reflection.md) and *generates* the `impl` — no
`#[derive]` compiler attribute, no per-trait codegen baked into the elaborator.

**Sketch.**

```fun
derive @ (Eq, struct x: I64; y: Bool end)
-- expands to:
impl Eq(struct x: I64; y: Bool end) = module
  fn eq(a, b) -> a.x == b.x && a.y == b.y end
end
```

The macro pattern-matches the record type's fields (name + field type), emits a
per-field `a.f == b.f` comparison for each, and `&&`-folds them. `Ord`, `Show`,
`Hash` follow the same structural recursion.

**Exploits.** type-case + record-type reflection (walk the fields) + traits (the
generated `impl` is just a structural dictionary).

**Feasibility.** The *structural walking* works today — type-case and record-type
reflection are both complete. The gap is on the **emit** side: `Decl` macros
currently only produce `DeclLet` bindings, not `impl` / `trait` bindings. So
`derive` can compute the right method bodies but cannot yet hand back an `impl`
declaration. Needs **`Decl` reflection widened** to cover `impl`/`trait`. This
directly advances [Trait library deriving and protocols](../tickets/design-trait-library-deriving-and-protocols.md)
and is the substantial payoff for [Stage 11](../tickets/specify-stage-11-macro-powered-language-features.md).

## 2. Type-*providing* `printf` / `format` *(best small demo)*

**Idea.** A `format` macro parses its format-string *literal at expansion time* and
computes its own result type from the holes it finds. The type is not written by
the user — the macro *provides* it.

**Sketch.**

```fun
format @ ("%d items, %s")   -- has type  I64 -> String -> String
```

`%d` contributes an `I64` parameter, `%s` a `String` parameter; the macro folds
these into a curried function type ending in `String`, and emits the matching
lambda. A different format string yields a different type — the type is a function
of the literal.

**Exploits.** type-providing macros (Stage 10, `R = RExpr(Type)` — the macro
returns both an expression and the type it inhabits) + compile-time inspection of
the string literal (`Atom`).

**Feasibility.** **High, and self-contained.** Everything needed exists: Stage 10
gives type-providing macros, and the format string is a literal `Atom` the macro
can read character-by-character at expansion. No reflection gaps. This is the
strongest *small* demonstration of what type-providing macros buy.

## 3. `match`-as-a-macro / pattern DSLs

**Idea.** Once `Match` is reflected in the `Expr` ADT, `match` (and patterns)
become things macros can construct and destructure — opening a family of
pattern-level DSLs built over the existing pattern reflection (`RawPatCon`,
`RawPatWild`, `RawPatBind`, …).

**Sketch.**

```fun
matches? @ (x, Some(_))          -- a Boolean tester macro: expands to
                                  --   match x do Some(_) -> True | _ -> False end
guard @ (cond, body)             -- when/guard sugar
if_ @ (c, t, e)                  -- a TRUE prelude-macro `if` (expands to match)
```

Also enables active / view patterns and other library-defined control forms, all
without new compiler nodes.

**Exploits.** pattern reflection (already partly present) + a reflected `Match`.

**Feasibility.** **Blocked** — macros cannot construct `Match` today; any node
other than `Var/Ap/Lam/Let/Atom` falls through to an opaque passthrough. This is
exactly the payoff of [Reflect Match in the Expr macro ADT](../tickets/reflect-match-in-expr-macro-adt.md).
The *true* prelude-macro `if` (see [Bool and `if` as library features](bool-and-if-as-library.md),
which explains why `if` is desugared in the enforester instead) lands here.

## 4. Compile-time-checked effect DSLs

**Idea.** A block macro — `statemachine do … end`, `pipeline do … end` — expands to
`perform`/handler code, and *validates the block's well-formedness at expansion*
(e.g. every declared state is reachable, no dangling transitions). Ill-formed DSL
programs fail to expand, not to run.

**Sketch.**

```fun
statemachine do
  start -> running
  running -> done
end
-- expands to perform/handler scaffolding; the macro rejects the block
-- at expansion time if (say) `done` is unreachable.
```

**Exploits.** macros + [algebraic effects](algebraic-effects.md) together — the macro *emits* the
effect scaffolding and *checks* the DSL.

**Feasibility.** **Medium.** The emitted body is mostly opaque passthrough of
effect nodes (`perform`/handler), so it does not depend on the `Match` reflection
gap; the analysis (reachability, etc.) is ordinary macro-time computation over the
parsed block. The main work is designing the surface block and the validation.

## 5. Macro as a partial evaluator for type-case *(the deep one)*

**Idea.** `type_case` is a *runtime* `match` on a `Type` value. But a macro that
already knows the type *statically* can pre-resolve that match at expansion —
erasing the runtime dispatch entirely and emitting the specialized branch
directly. Generic code becomes specialized code with no runtime cost.

**Punchline.** The dynamic type-case feature and the static macro feature are the
**same mechanism at two different stages**: matching on a `Type`. One does it while
the program runs; the other does it while the program is being built. A macro that
performs type-case at expansion is a partial evaluator for the runtime version.

**Exploits.** types-as-values (the `Type` a macro holds is the same `Type`
type-case matches on) + type-aware macros (the macro knows the use-site type).

**Feasibility.** **Research-flavored.** It touches the "[compile-time-only type-case
erasure](type-case-generic-programming.md)" non-goal that was explicitly deferred. Conceptually clean and
the most interesting long-term, but the least immediately actionable.

---

## Related

- [Stage 11 macro-powered language features spec](../tickets/specify-stage-11-macro-powered-language-features.md)
  — the umbrella these feed into.
- [Trait library deriving and protocols](../tickets/design-trait-library-deriving-and-protocols.md)
  — advanced by #1.
- [Reflect Match in the Expr macro ADT](../tickets/reflect-match-in-expr-macro-adt.md)
  — unblocks #3.
- [Bool and `if` as library features](bool-and-if-as-library.md) — the landed
  Stage 11 increment and the reason `if` is enforester-desugared today.
