# Bool and `if` as library features (Stage 11 flagship)

**Decision:** the flagship of Stage 11 ("macro-powered language features") is to
**demote language constructs that were hardwired in the compiler core down into
library-level definitions**, proving the type theory can carry its own surface
syntax rather than growing more built-in machinery. The first, landed increment
demotes `Bool` and `if`.

## What changed

- **`Bool` is now a nominal ADT**, defined in the prelude source
  (`elab_prelude.ml` `stdlib_source`): `pub type Bool = False | True;`. The old
  primitive atom (`Atom.Bool`) and primitive type (`Atom_ty.TBool`) are gone, as
  is the builtin `Bool` type binding in `init_ctx`. `true`/`false` are no longer
  keywords; source uses the constructors `True`/`False`.
- **Primitives never mention `Bool`.** Comparison/equality prims return `I64`
  (`1`/`0`); `<`,`>`,`<=`,`>=` were renamed to `lt_i64`/`gt_i64`/`le_i64`/`ge_i64`.
  The prelude wraps them with `i64_to_bool = fn(n) -> match n do 0 -> False | _ -> True end`
  so the surface operators (`<`, `==`, `!=`, …) and `not` return the `Bool` ADT.
  This keeps the runtime (`nbe.ml`) decoupled from the prelude's `Bool` nominal —
  prim reducers stay `Atom.t list -> Atom.t option`.
- **`if` is sugar for `match`.** `if c do t else e end` is desugared in the
  enforester (`enforest_forms.parse_if`) to `match c do True -> t | False -> e end`.
  The dedicated `Syntax.If` / `Surface.If` / `Core.If` nodes and the `FIf`
  elimination frame were deleted. This loses **no** dependent-type reasoning
  because the `FMatch` frame already mirrors `FIf` in unification, refinement, and
  quoting.
- **Macro reflection:** the reflected `AtomVal.BoolAtom` variant and the
  `Syntax.bool` smart-constructor were removed (booleans aren't atoms). The
  `Bool`-typed flag fields the reflected ADTs still expose (`RawLet`'s recursive
  flag, `DeclLet`'s public flag) are now the `Bool` ADT's `True`/`False`
  constructors, built via `Macro_eval.vcon_bool`; `syntax_nominals` gained a
  `bool` field.

## Two regressions found and fixed while landing

- **Effects in `match`/`if` branch bodies:** `eval_match_result` (`nbe.ml`) used a
  non-effect-aware path when a match had no effect branches, so a `perform` inside
  a branch body could not propagate to an outer handler. Since `if` is now a
  `match`, this affected every `perform` inside an `if` branch. Fixed by always
  evaluating the selected branch body effect-awarely.
- **Constructor patterns in tuples:** `match x do (True, y) -> …` failed scrutinee
  inference (`NotANominalType`) because `PatProd` gave every element a fresh meta
  without recursing. Fixed in `elab_match.refine_match_scrutinee_ty_opt` to refine
  each tuple element from its sub-pattern.

## Verified

778 tests green; REPL confirms `if True/False`, comparisons returning `Bool`,
`not`, `==`, type-case on `Bool`, `default[Bool]`, and `perform` inside `if`
branches under a handler.

## Design note — why not a *true* prelude macro for `if`?

A literal user-space `macro if` that expands to a `match` is **not yet possible**:
macros can only construct the reflected `Expr` ADT (`Var/Ap/Lam/Let/Atom`), which
does not include `Match`. So `if` is desugared in the enforester rather than in
prelude source. Reflecting `Match` (and patterns) in the `Expr` ADT is the natural
next increment toward fully library-defined control forms — see
[Reflect Match in the Expr macro ADT](../tickets/reflect-match-in-expr-macro-adt.md).

## Related / follow-on

- Demoting the **operator table** (`+`, `==`, `<`, …) out of `operator_env.ml`
  needs a new mechanism — the prelude is *not* currently an implicit syntax import,
  so prelude-declared operators do not reach user enforestation. See
  [Prelude-as-implicit-syntax-import for operator demotion](../tickets/prelude-implicit-syntax-import-operator-demotion.md).
- `&&` / `||` short-circuit operators — feasible now as builtin-table operators
  expanding to `match`. See
  [Add short-circuit && / || operators](../tickets/add-short-circuit-and-or-operators.md).
