# Record type reflection plan

## Goal

Extend runtime `Type` matching so generic code can inspect structural record/struct types by field shape. This completes the remaining type-case milestone after primitive and nominal type-head matching: generic functions should be able to dispatch on records that have particular fields and bind the field types for further type-case.

The intended surface is a struct-type pattern, not record-constructor pattern reuse:

```fun
match T do
| struct x: p; _ end -> ...
| struct x: I64; y: Bool; end -> ...
| _ -> ...
end
```

In `struct x: p; _ end`, the pattern binds `p` to the type of field `x`. The branch can then match on `p` or use it in refined expected types.

## Decisions

- Only `struct ... end` patterns are used for record type reflection.
- Do not reuse nominal record value-pattern syntax such as `Point {x; _}`; that syntax already matches record values.
- Struct type patterns only match `Type` scrutinees whose runtime value is a `VStruct` type.
- Field entries are matched by name, not position.
- `struct ...; _ end` is partial/open and matches structs with at least the listed fields.
- `struct ... end` without `_` is closed and requires exactly the listed visible fields.
- Field type annotations in the pattern are themselves type patterns:
  - `p` binds the field type;
  - `I64`, `Bool`, etc. match primitive type heads;
  - `Option(x)` matches nominal type heads and binds nominal parameters;
  - nested `struct ... end` type patterns should be supported if the parser/core representation makes it straightforward.
- Match only constructor field declarations (`x: T`) in struct types.
- Do not match `pub` value bindings or public methods in this phase, even though they may appear as visible struct members internally.
- Private struct members are not visible for matching.
- This phase should not introduce row polymorphism, field iteration APIs, field-name values, capability/module-signature reflection, or first-class reflection records.

## Phase 1: Syntax and AST ✓

Add a surface pattern form for struct type patterns in `lib/syntax/surface.ml`:

```ocaml
and struct_type_pat_field = string * pat

and pat =
  ...
  | PatStructType of {
      fields : struct_type_pat_field list;
      partial : bool;
    }
```

Update `lib/expand/enforest.ml` so pattern atoms accept:

```fun
struct x: p; y: I64; _ end
struct x: p; _ end
```

Parser rules:

- Entries are semicolon-separated.
- Named entries have `ID COLON pat`.
- `_` is only allowed as the final entry and sets `partial = true`.
- At least one named field is required.
- Duplicate field names should be rejected in elaboration, not by parser grammar.
- This is pattern syntax only; do not change expression-level `struct ... end` syntax.

Add syntax tests in `test/syntax/` for:

- open struct type pattern: `struct x: p; _ end`;
- closed struct type pattern: `struct x: I64; y: Bool end`;
- nested/nominal field type pattern: `struct value: Option x; _ end`.

## Phase 2: Core pattern representation ✓

Add a core pattern form in `lib/core_kernel/core.ml`:

```ocaml
| CPatStructType of {
    fields : (string * core_pat) list;
    partial : bool;
  }
```

This is a type-level pattern matched against `VStruct` values. The field subpatterns match the field type values, not record field values.

Update pattern traversals/comparison where needed:

- `lib/backend/interp/nbe.ml` `conv_pat`;
- any debug printer paths if core patterns are printed;
- any helper that recurses over `core_pat`.

## Phase 3: Elaboration ✓

Update `lib/semantic/typecheck/elaborate.ml` in `elaborate_pat_binders`:

- `PatStructType` is valid only when the scrutinee type is `VU`.
- Elaborate each field type subpattern against `VU`.
- Preserve binder order consistently with existing pattern binder order.
- Reject duplicate field names.
- Reject empty named field lists.
- Produce `CPatStructType { fields; partial }`.

Branch-sensitive refinement should treat a struct type pattern as replacing the matched type variable with a `VStruct` whose listed fields have refined/bound type values:

- For `struct x: I64; _ end`, refine the scrutinee type variable to a partial `VStruct` containing `x: I64`.
- For `struct x: p; _ end`, use a fresh meta for `x` during branch expected-type refinement and bind `p` to that field type in the branch context.
- Closed struct patterns can refine to `partial = false`; open patterns refine to `partial = true`.

If full dependent refinement for bound field types is too large for the first pass, keep Phase 1 elaboration scoped to branch-local binders and runtime matching, then add expected-type/context refinement as Phase 2. However, the target behavior should allow examples like:

```fun
field_default : (T : Type) -> I64 = fn(T) ->
  match T do
  | struct x: p; _ end -> match p do I64 -> 1 | _ -> 0 end
  | _ -> 0
  end
```

## Phase 4: Match compilation and runtime matching ✓

Update `lib/semantic/match/core_match_compile.ml` and `lib/semantic/match/core_decision_tree.ml` so struct type patterns can dispatch on the presence and type of fields.

Implemented first slice:

- `CPatStructType` uses direct runtime pattern matching when present in a match;
- open patterns only require listed constructor fields;
- closed patterns require the visible constructor field set to match exactly;
- field subpatterns are ordinary type patterns, so primitive and nominal type-head matching works inside field types.

Future optimization:

- compile struct type patterns into the decision tree by field occurrences instead of using the direct runtime matcher.

Update runtime matching in `lib/backend/interp/nbe.ml`:

- `match_core_pat` should match `CPatStructType` against `VStruct` values.
- Consider only constructor fields (`kind = Field`) when matching struct type patterns.
- Ignore public value bindings and public methods for this phase; they are module/capability members, not record fields.
- Private members are not visible for matching.
- For each listed field, look up the field type by name and match its type value with the field subpattern.
- Closed patterns fail if there are extra visible constructor fields.
- Open patterns ignore extra visible constructor fields.

Update decision-tree occurrence resolution if compiling nested field type patterns through child occurrences:

- add or reuse an occurrence shape for field type lookup by name;
- resolving a field occurrence from `VStruct` should return the field's type value.

## Phase 5: Tests and examples ✓

Add semantic tests in `test/semantic/test_elaborate.ml`:

- `struct x: p; _ end` binds `p : Type` and allows nested type-case on `p`.
- `struct x: I64; _ end` refines the field type enough for branch-local generic code.
- closed struct type patterns reject extra fields at runtime/semantic behavior level.
- duplicate field names in a struct type pattern are rejected.
- struct type patterns against non-`Type` scrutinees are rejected.

Add backend tests in `test/backend/test_core.ml`:

```fun
classify : Type -> I64 = fn(T) ->
  match T do
  | struct x: I64; _ end -> 1
  | struct x: Bool; _ end -> 2
  | struct y: p; _ end -> match p do String -> 3 | _ -> 4 end
  | _ -> 0
  end
```

Expected cases:

- `classify (struct x: I64; y: Bool; end)` -> `1`;
- `classify (struct x: Bool; end)` -> `2`;
- `classify (struct y: String; z: I64; end)` -> `3`;
- `classify I64` -> `0`.

Closed-vs-open behavior:

```fun
match struct x: I64; y: Bool; end do
| struct x: I64 end -> 1
| struct x: I64; _ end -> 2
| _ -> 3
end
```

Expected: `2`, because the closed first pattern rejects the extra `y` field.

## Verification

Run focused tests:

```sh
dune exec test/syntax/test_syntax.exe
dune exec test/semantic/test_elaborate.exe -- test dependent
dune exec test/backend/test_core.exe
```

Then full validation:

```sh
dune test
git diff --check
```

## Non-goals

- Matching nominal record constructor syntax in type patterns, e.g. `Point {x; _}`.
- Matching public value bindings or public methods.
- First-class reflection values for field names or field lists.
- Row-polymorphic APIs.
- Typeclass/protocol derivation.
- Compile-time-only reflection or macro integration.
- Structural equality for record values.
