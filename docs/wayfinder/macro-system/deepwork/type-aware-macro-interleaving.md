# Deepwork: Type-aware macro interleaving implementation

> **Historical work log (OCaml prototype).** Every path below — `lib/…`,
> `dune build`, `dune exec test/backend/…` — belongs to the OCaml prototype,
> which was removed on 2026-09-25; none of them resolve any more. The design it
> implements survives in
> [topics/macro-interleaving-design.md](../../topics/macro-interleaving-design.md)
> (parent: [TYPE_AWARE_INTERLEAVING.md](../TYPE_AWARE_INTERLEAVING.md)), and its
> ticket is closed. For what is built now, see [docs/STATUS.md](../../../STATUS.md)
> and the macro [STATUS.md](../STATUS.md). Covers Stages 1–6.

## Goal

Implement the design in `docs/17.type_aware_macro_interleaving_design.md`, starting with Stage 1: split parsed unresolved macro annotations from resolved macro kinds without changing runtime behavior more than necessary.

## Confirmed context

- Design ticket resolved in `docs/wayfinder/compiler-directions/tickets/01-type-aware-macro-interleaving.md`.
- Canonical design doc: `docs/17.type_aware_macro_interleaving_design.md`.
- Current `Syntax.MacroKind.t` is `Expr of string option * string option | Decl` in `lib/core_kernel/syntax.ml`.
- Macro kind values appear in both `Syntax.struct_binding.MacroBinding` and `Syntax.kind.MacroDef`, and in `Surface.MacroBinding` / `Surface.MacroDef`.
- Current parser/enforester resolves macro annotations in `lib/expand/enforest.ml:276-336`, using `Compiler_names.Type_name.macro_annotation_known`; Stage 1 should begin removing that semantic decision from parsing.
- Current macro registration and use sites read `Syntax.MacroKind` in `lib/expand/expand.ml`, `lib/expand/expand_ctx.ml`, `lib/loader/core_loader.ml`, and typecheck files.
- Tests with macro annotation behavior are concentrated in `test/backend/test_core.ml` around macro suites and cases for `Expr(A)`, `Expr(Foo)`, `Expr(Int)`, `Expr(I64)`.

## Stage 1 draft plan

Reviewed by oracle. Accepted Stage 1 boundary:

1. Add `Syntax.MacroAnnotation` as the parsed/unresolved representation.
2. Change `Syntax.MacroBinding.kind`, `Syntax.MacroDef.kind`, and Surface twins from `MacroKind.t option` to `MacroAnnotation.t option`.
3. Keep compiled/registry/cache side as existing `Syntax.MacroKind.t` for Stage 1.
4. Add one quarantined adapter from `MacroAnnotation.t` to old `MacroKind.t`, plus the existing implicit-parameter synthesis decision. The adapter may still use `Compiler_names.Type_name.macro_annotation_known` in Stage 1, but it must be the only place. Add a Stage 2 comment.
5. Update AST-preservation plumbing deliberately: lower/reverse-lower/template/surface rewrite/add_scope.
6. Update enforester annotation parsing to emit parsed annotations, then use the adapter for today's implicit binder parameter synthesis.
7. Update macro registration/validation call sites to resolve `MacroAnnotation.t` through the adapter before storing/using `MacroKind.t`.
8. Do not remove `known_type_names` yet; do not add semantic driver, resolved type refs, macro exports, cache identity, or macro-free output in Stage 1.
9. Run focused tests from `test/backend/test_core.exe` for macro annotation cases, then `dune build` or `dune test` if feasible.

## Review questions for oracle

- Is Stage 1 too broad if it changes every macro field to `MacroAnnotation.t` before a semantic driver exists?
- Should Stage 1 instead add `MacroAnnotation.t` plus parser conversion at the boundary, preserving existing `MacroKind.t option` fields until Stage 2?
- What is the lowest-risk staging that still moves toward the locked design and avoids another temporary semantic leak?

## Oracle review result

- Hidden parser side effect: `enforest.ml` does not just choose a kind; binder resolution also synthesizes an implicit type parameter. Do not relocate that out of the parser until the semantic driver exists.
- Change the AST fields now despite churn; otherwise the split remains nominal only and causes another pass through all preservation sites later.
- Quarantine the old hardcoded resolution in one adapter; Stage 2 swaps its source of type facts from hardcoded known names to the semantic context.
- Keep `MacroKind` option-pair shape in Stage 1; explicit resolved sum type belongs later.
- Watch existing annotation-loss sites (`surface_to_syntax.ml`, `enforest_template.ml`, `elab_surface_rewrite.ml`) and make every drop deliberate.

## Stage 1 implementation status

Implemented by fixer and reconciled locally.

Changed source files:
- `lib/core_kernel/syntax.ml`: added `MacroAnnotation` and `MacroAnnotationAdapter`; changed Syntax macro fields to `MacroAnnotation.t option`.
- `lib/syntax/surface.ml`: changed Surface macro fields to `Syntax.MacroAnnotation.t option`.
- `lib/expand/enforest.ml`: parses syntactic annotations and calls adapter only for legacy implicit-parameter synthesis.
- `lib/expand/expand.ml`: resolves annotations through adapter before storing `MacroKind.t` in macro registry.
- `lib/loader/core_loader.ml`: resolves public imported macro annotations through adapter before caching/registering.
- `lib/expand/lower_surface.ml`, `lib/expand/surface_to_syntax.ml`, `lib/semantic/typecheck/elab_surface_rewrite.ml`: documented deliberate annotation drops.
- `.gitignore` / `.ignore`: added local deepwork and Magic Context generated-directory ignores.

Validation run:
- `dune build` succeeded as first half of a chained command; the second half failed only because I used an invalid Alcotest argument form.
- `dune exec test/backend/test_core.exe -- test macros` passed: 113 macro tests.
- `dune test` passed with no output.

Pending review focus:
- Adapter lives in `syntax.ml` and references `Compiler_names`; acceptable short-term because both are core kernel, but review whether it is too semantically heavy for syntax.
- Comments on deliberate annotation drops should not overstate that these paths are already fully macro-free.

Oracle review after implementation found three Stage 1 blockers, all fixed:
- Preserved legacy `: NonExpr(...)` behavior via `MacroAnnotation.LegacyExprBinder` so the old unconditional binder synthesis remains available for that parser shape.
- Reworded annotation-drop comments to say the resolved kind is carried by macro registry/table after registration, rather than overclaiming macro-free output.
- Added explicit comments to `enforest_template.ml` annotation-drop sites and recorded annotated macro-generating macros as a Stage 2+ limitation.

Revalidation after fixes:
- `dune build` passed.
- `dune exec test/backend/test_core.exe -- test macros && dune test` passed.

## Stage 2 scope and seam map

Goal: complete migration-table Stage 2 from `docs/17.type_aware_macro_interleaving_design.md`: remove the static `known_type_names` semantic decision from macro annotation handling and make parser/enforester output unresolved annotations with uniform binder fallback. This is still pre-driver work; actual semantic type-namespace lookup belongs to Stage 4 (`MacroResolver.resolve_kind` against semantic context).

Stage 2 should include:
- Remove `Compiler_names.Type_name.macro_annotation_known` from the macro annotation adapter path.
- Make all uppercase annotation names passed through the adapter resolve as binder parameters for now, including names that were previously hardcoded known types (`I64`, `Bool`, `Int`, `List`, `Expr`, etc.).
- Keep `_` as unconstrained expression and lowercase/non-binder names as unconstrained expression, matching the existing parser fallback behavior.
- Keep `LegacyExprBinder` until the semantic driver owns implicit-parameter synthesis.
- Update/rename macro annotation tests whose labels say "known type/no binding".

Stage 2 should NOT include:
- Semantic module driver, queue, macro exports, semantic context fingerprint, cache identity, or import rework.
- Final resolved `MacroKind` sum type or semantic resolved type refs.
- Removal of `MacroAnnotationAdapter`; it remains the temporary bridge for registry callers.

Confirmed seams:
- `lib/core_kernel/syntax.ml:201-219`: remove the `known` list and `List.mem name known` branch from `MacroAnnotationAdapter.resolve`.
- `lib/core_kernel/compiler_names.ml:16-29`: remove `Type_name.macro_annotation_known` once no callers remain.
- Existing adapter callers are passive and should not need logic changes: `lib/expand/enforest.ml` (`resolve` for implicit-param synthesis), `lib/expand/expand.ml` (`resolve_kind_only` for registry), and `lib/loader/core_loader.ml` (`resolve_kind_only` for imported public macros).
- `test/backend/test_core.ml:1202-1244`: update old known-type/no-binding annotation test labels and add/adjust coverage for uppercase builtin names becoming binders.

Stage 2 draft implementation plan:
1. In `MacroAnnotationAdapter.resolve`, remove `macro_annotation_known` lookup and treat any uppercase non-underscore annotation name as a binder.
2. Update the adapter comment to say semantic annotation resolution is deferred to the future semantic driver / Stage 4 resolver.
3. Remove `Compiler_names.Type_name.macro_annotation_known` if `git grep` confirms no remaining references.
4. Rename the three old annotation tests whose names/descriptions assert "known type/no binding" to Stage 2 binder-oriented names.
5. Add at least one explicit Stage 2 test showing a builtin uppercase name such as `Bool`/`I64` is accepted as a binder rather than a parser-known constraint.
6. Validate with focused macro annotation/constraint filters, full macro suite, `dune build`, and `dune test`.

## Stage 2 implementation status

Implemented by fixer. All changes are source-only; no semantic driver or new infrastructure.

### Oracle correction applied

Stage 2 is an intentional behavior flip: `Compiler_names.Type_name.macro_annotation_known` is removed from the annotation adapter, and ALL leading-uppercase annotation names now resolve as binder parameters with synthesized implicit params. This means names like `I64`, `Bool`, `Int`, `Str`, etc. — which were previously treated as type constraints with no implicit binding — are now uniform binders. The semantic type-constraint path is deferred to Stage 4's `MacroResolver.resolve_kind` against the semantic type namespace.

### Changed files

| File | Change |
|------|--------|
| `lib/core_kernel/syntax.ml` | Removed `List.mem name known` branch and `Compiler_names.Type_name.macro_annotation_known` import from `MacroAnnotationAdapter.resolve`. Updated adapter comment to Stage 2 contract: `_` → unconstrained, uppercase → binder, lowercase → unconstrained. |
| `lib/core_kernel/compiler_names.ml` | Removed `Type_name.macro_annotation_known` (no callers remain). |
| `test/backend/test_core.ml` | Renamed 5 test functions and their Alcotest labels from constraint/known-type wording to binder wording. Added 2 new tests: `test_binder_i64_referenceable` (proves `Expr(I64)` binder is bound/referenceable in body) and `test_binder_typo_guard` (proves `Expr(Intt)` unresolved uppercase name remains a binder, not a guessed constraint/error). Old constraint mismatch tests renamed to binder mismatch tests. |
| `TODO.md`, `docs/STATUS.md`, `docs/plan-for-macros/STATUS.md` | Updated canonical status/TODO language so it no longer describes the removed static known-type list as current. These now say Stage 1 and Stage 2 are complete, and the remaining work is semantic driver/resolver-based constraint recognition against the current prior type namespace. |

### Added tests

1. **`Expr(I64) binder is referenceable`** — `macro mk(_) : Expr(I64) do do _ = I64; Syntax.i64(1) end end` → 1L. Proves the synthesized binder `I64` can be referenced in the macro body.
2. **`Expr(Intt) typo → binder not constraint`** — `macro mk(_) : Expr(Intt) do do _ = Intt; Syntax.i64(1) end end` → 1L. Proves unresolved uppercase names are binders, not guessed as constraints/errors.

### Validation

- `dune build` passed.
- `dune exec test/backend/test_core.exe -- test macros` passed: 115 tests (was 113).
- Full macro suite green; no regressions.
- Orchestrator revalidated after fixer output with `dune build && dune exec test/backend/test_core.exe -- test macros && dune test`; all passed. The later doc-only status updates do not require rerunning OCaml tests.

## Stage 3 scope and seam map

Goal: add `Macro_driver` as an additive skeleton with no behavioral change. The driver runs the expander over top-level module bindings wholesale and collects compiled macro exports. It is NOT incremental; per-binding semantic advancement belongs to Stage 4.

### Stage 3 implementation status

Implemented by fixer. Added a new module `Macro_driver` in `lib/semantic/typecheck/macro_driver.ml` and the corresponding tests.

#### Changed files

| File | Change |
|------|--------|
| `lib/expand/expand.ml` (line 625) | Exported `expand_struct_bindings` as a public function so the driver can call it directly on an existing `Expand_ctx`. |
| `lib/semantic/typecheck/macro_driver.ml` (new, ~110 lines) | Additive driver skeleton. Defines `macro_export`, `driver_output`, and `run`. Initialises `elab_ctx` + `expand_ctx` with callbacks matching `eval_decl_module`, calls `Expand.expand_struct_bindings` wholesale, rebuilds lowered surface, collects `macro_exports` sorted by name, copies macros into `elab_ctx.macro_table`. |
| `test/backend/test_core.ml` | Added 5 Stage 3 tests: driver-pipeline equivalence (runtime module, module with macro), macro_exports for default/Decl kinds, and elab_ctx.expand_ctx populated check. Added `run_driver` and `driver_vs_pipeline` helpers. |

#### Added tests

1. **driver equiv runtime module** — `pub x : I64 = 42; pub y = x + 1` produces identical binding structure from both `Macro_driver.run` and `Parse_expand.parse_module_with_ctx`.
2. **driver equiv module with macro** — module with a macro definition + call produces identical binding structure from both paths.
3. **driver macro_exports default kind** — `macro mk(_) -> Syntax.i64(1)` → export with name "mk" and default Expr kind.
4. **driver macro_exports Decl kind** — `macro gen(_) : Decl do Nil end` → export with name "gen" and Decl kind.
5. **driver elab_ctx.expand_ctx populated** — verifies `output.elab_ctx.expand_ctx` is `Some _`.

#### Validation

- `dune build` passed.
- `dune exec test/backend/test_core.exe -- test macros` passed: 120 tests (was 115, +5 new).
- All 5 driver tests passed.
- Full macro suite green; no regressions.

### Oracle phase review

Oracle approved Stage 2 with no blockers before Stage 3. It confirmed the source adapter contract, removal of `macro_annotation_known`, new/renamed tests and registrations, and status/TODO docs. Carry-forward note: there is now intentionally zero live constraint resolution from macro annotations until Stage 4 reintroduces the constraint shape via the semantic resolver. Stage 3 should not be expected to restore constraints; add positive constraint-resolution coverage in Stage 4.

## Stage 3 scope and seam map

Goal: implement migration-table Stage 3 from `docs/17.type_aware_macro_interleaving_design.md`: a callable semantic module driver skeleton with the queue shape, but no behavior replacement yet.

Narrowed boundary:
- Add a new additive driver module near the elaboration/loader boundary (`lib/semantic/typecheck/macro_driver.ml`).
- The driver should accept an already parsed/enforested `Syntax.t` module and expose an explicit source-order queue loop over top-level `Syntax.struct_binding` values.
- For Stage 3 only, each queued binding is passed through the existing expansion/lowering machinery and accumulated into a final `Surface.Module`; the old pipeline remains the behavioral authority.
- The driver output should include the lowered surface module, the `Expand_ctx.t`, the elaboration context used for callback setup, and a first-class `macro_exports` list populated from the expansion context's macro tables.
- Stage 3 tests should prove the driver is callable and preserves current behavior for simple modules and current macro registration/expansion behavior.

Stage 3 must NOT include:
- `MacroResolver.resolve_kind`, semantic type lookup, or restored constraint resolution.
- Replacing `Parse_expand.parse_module_with_ctx`, `Core_loader.visit_macros`, or import loading.
- Real per-binding elaboration / incremental `Elab_ctx` mutation if that would require a larger semantic refactor. This belongs to Stage 4+ when the resolver needs prior semantic context.
- Macro-generated declaration re-entry semantics beyond preserving current `Expand.expand_struct_binding` behavior; queue re-entry is Stage 6.
- Recursive macro cells, fuel guards, cache fingerprints, or final `MacroKind` sum-type replacement.

Implementation sketch:
1. Create `Macro_driver` with types:
   - `macro_export = { name; kind : Syntax.MacroKind.t; compiled : Core.value; syntax_nominals : Macro_eval.syntax_nominals option }`
   - `driver_output = { surface : Surface.t; expand_ctx : Expand_ctx.t; elab_ctx : Elab_ctx.Ctx.t; macro_exports : macro_export list }`
2. Provide `run : ?loader:Core_loader.t -> Syntax.t -> driver_output`.
3. Initialize `elab_ctx = Elaborate.init_ctx ()`, `syntax_nominals = Elaborate.syntax_nominals elab_ctx`, and an `Expand_ctx.t` with callbacks equivalent to `eval_decl_module`: `elaborate` uses `Elaborate.on_expr ?loader elab_ctx`, `eval_and_apply` uses `Nbe.apply`.
4. Queue over module/struct bindings in source order. For Stage 3, call `Expand.expand_struct_binding` on each queued binding and append non-`MacroBinding` expanded bindings to runtime output, preserving the existing macro-binding removal behavior. Track active scopes from introduced scopes, mirroring `Expand.expand_struct_bindings`.
5. Lower the accumulated expanded syntax bindings via `Lower_surface.lower_expr` by rebuilding a `Syntax.Module`/`Syntax.Struct`, or via existing lowering helpers if accessible.
6. Extract macro exports from `expand_ctx.macro_table` + `macro_kind_table` after the queue completes.
7. Add backend smoke tests that parse source with `Enforest.parse_module`, call `Macro_driver.run`, and assert output shape and macro export names/kinds. Avoid tests that imply semantic constraint restoration.

Open risks for review:
- `Expand.expand_struct_binding` is currently an internal recursive function in `expand.ml`, but because modules are flat and no `.mli`, it is callable from `Macro_driver`; confirm this is acceptable or whether to add a small explicit wrapper.
- Calling `Expand.expand_struct_binding` per binding from another module must mirror `expand_struct_bindings`' scope accumulation exactly (`add_struct_binding_scopes active_scopes` before expansion, append flattened introduced scopes after expansion).
- Driver output's `elab_ctx` is callback setup state in Stage 3, not yet the incrementally advanced semantic context; name/comment must avoid overclaiming.

### Oracle plan review corrections

Oracle approved the additive Stage 3 boundary, but changed the implementation shape:

Required corrections before implementation:
1. Do **not** hand-copy the per-binding scope-accumulation queue loop yet. Stage 6 owns real generated-declaration queue re-entry. For Stage 3, call `Expand.expand_struct_bindings ctx bindings` wholesale so behavior exactly matches the current expander.
2. Add structural-equivalence tests comparing `Macro_driver.run source_stx` surface output against the current `Parse_expand.parse_module_with_ctx` output for representative modules. Shape-only tests are insufficient.
3. The expander `elaborate` callback must return a `Core.value`: call `Elaborate.on_expr ctx expr`, then `Elaborate.Ctx.eval ctx core`, matching `eval_decl_module`.
4. Build `macro_exports` from `expand_ctx.macro_table`; for each name, look up kind in `macro_kind_table` with `Syntax.MacroKind.default` fallback.
5. Preserve the original parsed module node/span when reconstructing lowered output: `{ original_stx with kind = Syntax.Module { bindings = expanded_bindings } } |> Lower_surface.lower_expr`.
6. Drop `?loader` from Stage 3's public signature rather than half-wiring imports. Import unification remains Stage 8.

Should-fix implementation notes:
- Make `driver_output.elab_ctx` honest by copying expanded macros into `elab_ctx.Ctx.macro_table` and setting `elab_ctx.Ctx.expand_ctx <- Some expand_ctx`, matching `eval_decl_module`; still comment that it is not an incrementally advanced per-binding context.
- Define Stage 3 `macro_exports` as macros defined by this module in the no-loader path. Later stages must separate local exports from imported macros.
- `Elaborate.syntax_nominals elab_ctx` is acceptable and cleaner than hand-building the nominals record.

## Stage 3 implementation status

Implemented and reconciled locally.

Changed source files:
- `lib/semantic/typecheck/macro_driver.ml`: new additive Stage 3 driver skeleton. It accepts `Syntax.Module`, initializes an elaboration context and expand context with the same macro-compilation callbacks as `eval_decl_module`, calls `Expand.expand_struct_bindings` wholesale, lowers by preserving the original module node/span, collects deterministic `macro_exports`, copies macros into `elab_ctx.macro_table`, and sets `elab_ctx.expand_ctx`. The file explicitly notes that this is not an incrementally advanced per-binding semantic context yet.
- `test/backend/test_core.ml`: added Stage 3 macro-driver tests. Two tests compare full lowered `Surface.t` structural equality between the new driver and the existing `Parse_expand.parse_module_with_ctx` pipeline; three tests cover default/Decl macro exports and `elab_ctx.expand_ctx` population.

Reconciliation fixes after fixer output:
- Removed a redundant self-recursive wrapper for `Expand.expand_struct_bindings`; the existing top-level function is already externally callable in this no-`.mli`, `(wrapped false)` library.
- Tightened equivalence tests from binding-name summaries to full `Surface.t` structural equality, per oracle's required correction.

Validation after reconciliation:
- `git diff --check` passed.
- `dune build` passed.
- `dune exec test/backend/test_core.exe -- test macros` passed: 120 macro tests.
- `dune test` passed: 308 tests.

## Stage 4: Oracle plan and corrections (as recorded before implementation)

Scope Stage 4 to module-level macros in `Macro_driver.run` with initial/prelude semantic context only. Key decisions:

1. Kind resolution and parser-synthesized implicit params MUST move together in the driver path — a kind-only pre-registration is incoherent.
2. Use a narrow known-type predicate: `Elab_resolve.resolve_path_value_opt ctx [] name`, then `Elab_ctx.Ctx.conv ctx ty VU`. Do NOT use `Elab_validate.is_type_like_value`.
3. Pre-register resolved macro kinds in the driver before wholesale expansion, and guard `expand.ml` so pre-registered kinds are not overwritten at the module `MacroBinding` registration site. Also guard `MacroDef` if straightforward, but document expression-position macro resolution as out of Stage 4.
4. If the parser synthesized a binder but the semantic resolver returns no param (constraint), strip the leading parser-synthesized Lam from the macro value.
5. Do NOT implement per-binding semantic advancement or user-defined prior type ordering.

## Stage 4 implementation status

Implemented by fixer. Added semantic macro annotation resolution against the initial prelude context, Lam stripping for constraint annotations, and kind-locking to prevent the adapter from overwriting driver-resolved kinds.

### Changed files

| File | Change |
|------|--------|
| `lib/semantic/typecheck/macro_resolver.ml` (new, ~80 lines) | Semantic annotation resolver. Uses `Elab_resolve.resolve_path_value_opt ctx [] name` + `Elab_ctx.Ctx.conv ctx ty VU` to check known types. Returns `(MacroKind.t, param option)`. Constraint for known types, binder for unknown uppercase, unconstrained for `_`/lowercase. |
| `lib/semantic/typecheck/macro_driver.ml` | Stage 4 additions: `pre_register_macros` function iterates top-level `MacroBinding` values, calls `Macro_resolver.resolve_kind`, pre-registers + locks semantic kinds, strips parser-synthesized Lams when semantic result is a constraint. Helper functions `parser_synthesized_binder` and `strip_leading_lam`. |
| `lib/expand/expand_ctx.ml` | Added `macro_kind_locked : (string, unit) Hashtbl.t` field. Added `lock_macro_kind` and `is_macro_kind_locked` functions. Updated `create` and `copy`. |
| `lib/expand/expand.ml` | Two guard sites (MacroDef line ~327, MacroBinding line ~532): only call `register_macro_kind` if not `is_macro_kind_locked`. |
| `test/backend/test_core.ml` | Added 5 Stage 4 tests: driver Expr(I64) constraint kind, driver constraint no binder arity, driver Expr(Intt) binder kind, driver Expr(foo) unconstrained, driver Expr(_) unconstrained. Added `exported_kind` helper. |

### Added tests

1. **Expr(I64) constraint kind** — `Expr(None, Some "I64")`, `has_type_binding = false`, `type_constraint_name = Some "I64"`.
2. **constraint no binder arity** — driver expands module with `: Expr(I64)` constraint macro and runtime binding; surface is well-formed with `x` binding present.
3. **Expr(Intt) binder kind** — `Expr(Some "Intt", None)`, `has_type_binding = true`.
4. **Expr(foo) unconstrained** — lowercase → `has_type_binding = false`.
5. **Expr(_) unconstrained** — wildcard → `has_type_binding = false`.

### Oracle review (Stage 4)

APPROVE with two documented must-fix-before-wiring conditions:

1. **Bare-name/global-lock desync:** `pre_register_macros` locks all macro kinds by bare name before any expansion, using `Expand_ctx.macro_kind_locked`. Duplicate/shadowed top-level macro names produce kind/value desync; inner-scope `MacroDef` name reuse is suppressed. Safe only because the driver is test-only. Must be replaced by per-binding kind registration when the driver is wired (Stage 5/6).

2. **Lam-strip safety:** `strip_leading_lam` fallback (`_ → value`) leaves value unchanged when the leading node is not a `Lam`. Caller preconditions prevent this under normal parser behavior, but a comment documents the silent-arit-kind-desync risk.

### Validation

- `git diff --check` clean
- `dune build` clean
- `dune exec test/backend/test_core.exe -- test macros` passed: 125 tests (was 120, +5 driver tests)
- `dune test` passed: 313 tests
- All 10 driver tests (5 Stage 3 + 5 Stage 4) pass
- Oracle review: APPROVE (2 documented caveats)

## Stage 5 planning

Design doc says: "Canonical macro registration in driver; wrap old paths. One registration path."

### Code seams (from explorer)

1. **`Elaborate.infer` Module case** (`elab_infer.ml:287-501`): `go` loop processes bindings sequentially, threading `ctx`. `MacroBinding`/`MacroCallBinding` are currently **skipped** (line 290-291). Other bindings return `ctx'` via `Ctx.define`/`Ctx.bind`.

2. **Context mutation**: `Ctx.t` is an immutable record. `macro_table` is a **shared mutable hashtable** — mutations in one copy affect all copies. `expand_ctx` is a `mutable` field set via `<-`.

3. **`expand_struct_bindings`** (`expand.ml:432-446`): Sequential `go` loop with `active_scopes` accumulation. Could be called per-binding but would need refactored scope tracking.

4. **`Expander.elaborate` callback**: Defined in `Macro_driver.run` as a closure over `elab_ctx`. Per-binding advancement would need to update this callback each iteration.

5. **`eval_decl_module`** (`test_core.ml:847-876`): Calls `Parse_expand.parse_module_with_ctx` (parse→expand→lower in one call), then copies macros to `elab_ctx.macro_table`, then `Elaborate.on_expr ctx expr`.

### Key boundary question

Two interpretations of Stage 5 scope:

**Option A (narrow)**: Replace batch pre-registration with per-binding atomic kind+value registration. Keep prelude-only `elab_ctx` (no per-binding advancement). Remove `macro_kind_locked`. Tests: verify per-binding resolution works with prelude types only. User-type ordering deferred to Stage 6.

**Option B (full)**: Per-binding semantic advancement — after each non-macro binding, elaborate it to advance `elab_ctx` so subsequent macro annotations resolve against user-defined types. This requires extracting `ctx'` from `infer`'s Module `go` loop, or calling `infer` on individual bindings.

### Oracle review: APPROVE_OPTION_A with mechanism correction

Instead of a driver-level per-binding loop + lock table, inject a `resolve_macro_kind` callback into `Expand_ctx.t`. The expander's MacroBinding case calls it for semantic kind resolution, eliminating the lock table entirely.

**Mechanism**:
1. Add `mutable resolve_macro_kind : (MacroAnnotation.t -> MacroKind.t * param option) option` to `Expand_ctx.t`
2. In `expand.ml` MacroBinding: if callback is set, use it; else fall back to adapter. Register kind+value atomically in source order.
3. Move `parser_synthesized_binder` and `strip_leading_lam` from driver into `expand.ml` (syntactic helpers)
4. Delete `macro_kind_locked`, `lock_macro_kind`, `is_macro_kind_locked`, both guard sites, and `pre_register_macros`
5. Driver `run`: set `expand_ctx.resolve_macro_kind <- Some (fun ann -> Macro_resolver.resolve_kind elab_ctx ann)`, keep wholesale expand
6. MacroDef stays on adapter (expression-position deferred)
7. Tests: 5 existing driver tests pass through callback; add duplicate-name desync regression; add negative user-type test

### Stage 5 implementation plan

1. `lib/expand/expand_ctx.ml`: Add `resolve_macro_kind` mutable field. Delete `macro_kind_locked`, `lock_macro_kind`, `is_macro_kind_locked`. Update `create` and `copy`.
2. `lib/expand/expand.ml`: Move `parser_synthesized_binder` and `strip_leading_lam` from driver. In MacroBinding case: use callback if set, strip Lam if needed. Remove lock guards from both MacroBinding and MacroDef.
3. `lib/semantic/typecheck/macro_driver.ml`: Delete `pre_register_macros`, `parser_synthesized_binder`, `strip_leading_lam`. Set `expand_ctx.resolve_macro_kind` callback. Keep wholesale expand.
4. `test/backend/test_core.ml`: Keep existing 5 driver tests. Add duplicate-name test (later-wins kind+value). Add negative user-type-ordering test (`type MyTag = I64` above doesn't constrain).
5. Update deepwork, status docs, TODO.

## Stage 5 implementation status

Implemented per oracle-approved plan (Option A with callback mechanism).

### Changed files

| File | Change |
|------|--------|
| `lib/expand/expand_ctx.ml` | Added `resolve_macro_kind` field (mutable, option). Removed `macro_kind_locked` field, `lock_macro_kind`, `is_macro_kind_locked`. File: 129→120 lines. |
| `lib/expand/expand.ml` | Added `parser_synthesized_binder` and `strip_leading_lam` helpers. MacroBinding case now calls `resolve_macro_kind` callback + adapter fallback, strips Lam when semantic resolution says constraint. MacroDef lock guard removed (unconditional adapter registration). File: 627→653 lines. |
| `lib/semantic/typecheck/macro_driver.ml` | Deleted `pre_register_macros`, `parser_synthesized_binder`, `strip_leading_lam`. Injected `resolve_macro_kind` callback. Kept wholesale expand. File: 159→86 lines. |
| `test/backend/test_core.ml` | Added 2 Stage 5 tests: duplicate-name later-wins, prior-user-type-not-constraint. 127 macro tests (was 125). |

### Added tests

1. **duplicate macro name** — two macros with same name, second's kind wins (binder then constraint → constraint). Verifies kind+value registered atomically.
2. **prior user type not constraint** — `type MyTag = I64` above `: Expr(MyTag)` still resolves as binder. Documents Stage 6 boundary.

### Validation

- `git diff --check` clean
- `dune build` clean
- `dune exec test/backend/test_core.exe -- test macros` passed: 127 tests (all 5 Stage 4 driver + 2 new Stage 5)
- `dune test` passed: all suites green

### Oracle phase review (Stage 5): APPROVE with cosmetic fix (STATUS.md header bump)

## Stage 6 planning

Design doc says: "Macro-generated declaration re-entry into queue. Generated code processed inline."

### Current behavior

`expand_struct_bindings` (expand.ml:449-463) uses a `go` loop. When a `MacroCallBinding` (Decl macro) generates bindings via `unwrap_stx_decl`, those bindings are returned from `expand_struct_binding` and processed by the loop:
- `MacroBinding` nodes are **filtered out** at line 457 (correct for surface output — they're compile-time)
- Non-MacroBinding nodes (LetBinding, TypeBinding, etc.) go into `acc`
- Generated `MacroBinding` nodes are **never individually processed through `expand_struct_binding`**, so:
  - Their annotations are never resolved by the callback
  - They are never compiled and registered in expand_ctx
  - They are effectively lost (neither on surface nor in context)

### Proposed mechanism

In `expand_struct_bindings`'s `go` loop, after a `MacroCallBinding` generates bindings:
1. Separate generated `MacroBinding` nodes from other generated bindings
2. Process each generated `MacroBinding` through `expand_struct_binding ctx binding` individually — this fires the callback, compiles the macro, and registers kind+value
3. Generated `MacroBinding` nodes are still filtered out of `acc` (compile-time only, as before)
4. Other generated bindings go into `acc` as before

### Key question for oracle

This changes behavior for ALL callers of `expand_struct_bindings`, not just the driver. Currently generated `MacroBinding` nodes are silently dropped. With this change, they'd be compiled and registered. Is this a safe scope expansion, or should the generated-macro processing be gated behind a flag/only fire when `resolve_macro_kind` callback is set?

### Test plan

Add a driver test: Decl macro generates a macro with `: Expr(I64)` annotation. Assert the generated macro's export has constraint kind. End-to-end: generate, compile, use.

### Alternative (narrower)

Only process generated MacroBinding nodes when `resolve_macro_kind` is `Some` (driver-only). In the non-driver path, keep the current drop behavior. This avoids blast radius to existing tests.

### Oracle review (Stage 6): APPROVE

Approved the mechanism: a shared `expand_struct_bindings_with_scopes` helper that recursively re-enters generated declaration lists through the binding-list expansion loop. Oracle noted one non-blocking observation: generated binding lists start with fresh active scopes and do not inherit outer previous sibling scopes; this is consistent with macro hygiene unless later design changes it.

## Stage 6 implementation status

Implemented by fixer. Decl macro generated declaration lists are recursively re-entered through a shared `expand_struct_bindings_with_scopes` helper, so generated `MacroBinding` nodes compile/register, generated `MacroCallBinding` nodes recurse, generated siblings thread scopes to each other, and introduced scopes propagate outward. Top-level `expand_struct_bindings` filters `MacroBinding` only at the final surface-output boundary.

### Changed files

| File | Change |
|------|--------|
| `lib/expand/expand.ml` | Extracted `expand_struct_bindings_with_scopes` shared helper. Both top-level `expand_struct_bindings` and the Decl-macro generated-binding path call it, so generated `MacroBinding` nodes compile/register via callback, generated `MacroCallBinding` nodes recurse, scopes thread among generated siblings, and introduced scopes propagate outward. Top-level entry point filters `MacroBinding` only at the final surface-output boundary. |
| `test/backend/test_core.ml` | Added 2 Stage 6 tests: `test_generated_macro_binding_reentered` (Decl macro generates a `MacroBinding` node → exported with resolved constraint kind) and `test_generated_multi_binding_scope_threading` (Decl macro generates a `MacroBinding` then a `MacroCallBinding` that uses it → scoped correctly). |

### Added tests

1. **generated macro binding re-entered** — `Decl` macro generates `macro mk(_) : Expr(I64) do ... end` inline. The generated `MacroBinding` is compiled and registered, producing an export with constraint kind. Driver output includes the resolved macro export.
2. **generated multi-binding scope threading** — `Decl` macro generates a `MacroBinding` then a `MacroCallBinding`. The generated `MacroCallBinding` can reference the generated macro. Verifies that sibling bindings produced by a `Decl` macro thread scopes to each other.

### Validation

- `git diff --check` clean
- `dune build` clean
- `dune exec test/backend/test_core.exe -- test macros` passed: 130 tests (was 127, +2 new Stage 6 tests + 1 helper test)
- `dune test` passed: all suites green
- Oracle review: APPROVE (no blockers; non-blocking hygiene note about fresh scope inheritance)
