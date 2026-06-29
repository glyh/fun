# Handover — Stages 8-10 + infrastructure

## Completed

### Stage 8: Kind-Tagged Macros
- `MacroKind = Expr(binding, constraint) | Decl` — annotation determines kind
- `: Decl` → Decl macro, `: Expr(_)` / `: Expr(A)` / `: A` → Expr macro
- Context validation at call sites (Decl-in-Expr rejected, etc.)
- `MacroCallBinding` in module/struct declaration position
- Decl macros return multiple bindings via `struct_binding list`
- Import support via `eval_and_apply` threading + `macro_cache` kind preservation

### Stage 9: Decl/Pattern ADT
- **Decl ADT**: `DeclLet(Id, Expr, Bool)`, `Decls = List(Decl)` (replaced ad-hoc `DeclNil|DeclCons`)
- **Pattern ADT**: `RawPatWild|RawPatBind|RawPatCon|RawPatAtom|RawPatProd|RawPatOr` (6 ctors)
- Builders: `Syntax.decl_let`, `Syntax.pat_wild`, `pat_var`, `pat_con`, etc.
- `wrap_stx_decl`/`unwrap_stx_decl`, `wrap_stx_pat`/`unwrap_stx_pat`
- Pattern synonyms for all Pattern constructors
- Generic `List(A)` type in stdlib prelude

### Stage 10: Type-Aware Macros
- `: A` / `: Expr(A)` binds `A` to expected type (compiled as implicit type param)
- `: Expr(_)` — wildcard, no binding
- `: Expr(I64)` — constraint slot set (runtime enforcement deferred)
- Known-type disambiguation: `known_type_names` list (`I64`, `Bool`, `Option`, etc.)  
- `Syntax.Type = ExprType(Type) | DeclType | DeclsType | PatternType(Type)` ADT in prelude
- Expander defers type-binding macros to elaborator (args in `Stx`/`StxExpr`)
- `MacroDef` omits `Fun.protect` for type-binding macros
- `Ctx.expand_ctx` (mutable) + `Ctx.macro_table` in elaboration context
- `infer` path: evaluates macro with fresh metavariable as `A`
- `check` path: evaluates macro with expected type as `A`
- Tests: type-directed defaults, positive/negative type constraint, disambiguation

### Infrastructure
- `parse_spec.ml` — combinator library (`seq`, `alt`, `opt`, `eof`, `punct`, `ident`, etc.)
- 7 parser migrations to specs
- `parse_fn_parts` rest fix (3 locations)
- `Parse_error` module + accumulator in env + `skip_to_statement_boundary`
- `[@@deriving show]` on `Raw_syntax` types
- `parse_fn_parts` arrow body returns proper rest (not hardcoded `[]`)
- `unwrap_stx_decl` option-typed recursion (empty list ≠ invalid)
- `Lower_surface`/`expand.ml` scope-addition preserves `kind` field (3 locations)

## Remaining work

### High priority
- **Constraint enforcement**: DONE. `: Expr(I64)` now validates expected type at elaboration.
  - `elab_infer.ml`: resolves constraint type name v, unifies fresh metavariable with it
  - `elab_check.ml`: resolves constraint type name, unifies expected type with it
  - `test_constraint_rejects_mismatch` validates negative case
  - Also: `main.ml` and `eval_with_imported_macros` now properly transfer
    expander macro_table to elaboration context (required for type-binding macros)

- **Enforest line count**: N/A — limit is 3000, not 1500 (verified in test_line_counts.ml).
  Enforest.ml at 1737 lines is well within limit.

### Medium priority
- **Stage 11**: Macro-Powered Language Features — derived helpers, DSL blocks, scaffolding.
- **Stage 12**: Macro Diagnostics & Expansion UX.

### Low priority
- **Disambiguate by scope** (not just `known_type_names`): `: Expr(MyAwesomeType)` with `type MyAwesomeType = I64`
  defined should recognize it as a known type, not a binder. Parser has no module scope access.
  
- **Nested-module ADT constructor resolution**: `pub pattern PatWild = RawPatWild(_)` inside modules
  fails because `scan_env` matches by type name, not constructor name. Affects pattern matching
  in macro bodies for module-scoped ADTs. Root cause: `find_nominal_template_opt` doesn't
  scan constructor lists when the scrutinee is `VNominal` (the `| _ ->` path in `elab_patterns.ml`).

## Architecture notes

- **Expander vs Elaborator**: macros with `has_type_binding` are deferred from expander to elaborator.
  The expander wraps args in `Stx` (survives lowering as `StxExpr`). The elaborator uses
  `ctx.macro_table` (populated from expander) and `ctx.expand_ctx` (set during `load_elaborated`/`eval_*`).

- **MacroKind.Expr**: `(binding: string option, constraint: string option)`.
  Both `None` = wildcard. Binding set = type-binding param created. Constraint set = known type name
  (runtime enforcement deferred).

- **AGENTS.md**: recurring patterns, conventions, gotchas documented.

## Test counts
- Syntax: 133
- Semantic: 310  
- Backend: 300
- Total: 743
