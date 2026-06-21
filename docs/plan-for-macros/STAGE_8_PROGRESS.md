# Stage 8: Problem-Aware Macros — Implementation Progress

## Status: Implemented (core infrastructure)

The problem kind is visible to macros via an implicit `[why]` parameter.

## What's implemented

### `Syntax.Problem` ADT (stdlib)
```fun
pub type Problem = ExprProblem | PatternProblem | DeclProblem
```
Namespaced to avoid shadowing `Syntax.Expr` (the expression ADT).

### Macro syntax
```fun
macro check[why](stx) -> match why do | Syntax.ExprProblem -> Syntax.i64(1) | _ -> Syntax.i64(0) end
check @ (0)    -- returns 1 in expression context
```
- `[...]` bracket detected by `parse_macro_binding`, sets `has_problem = true`
- Parameter name is arbitrary (`[ctx]`, `[p]`, etc.)

### Type changes
| Type | Field added |
|------|-------------|
| `Syntax.MacroBinding` | `has_problem : bool` |
| `Syntax.MacroDef` | `has_problem : bool` |
| `Surface.MacroBinding` | `has_problem : bool` |
| `Surface.MacroDef` | `has_problem : bool` |

### Expand context
- `Expand_ctx.current_problem : value option` — set by elaborator/caller
- `Expand_ctx.register_macro_with_problem` — stores macro with problem flag
- `Expand_ctx.macro_has_problem ctx name` — checks flag at expansion time
- `parse_expand` accepts `~current_problem` parameter

### Expansion
When expanding `MacroCall(f, args)`:
1. Check `macro_has_problem`
2. If true, prepend `current_problem` value as first (implicit) argument
3. Apply folded left: implicit args first, then wrapped explicit args

### Test helper
Resolves `Syntax.Problem` nominal, constructs `VCon "ExprProblem"`, passes via `~current_problem`.

## What works

- [x] `macro check[why](stx) -> ...` parses correctly in both module and do-block contexts
- [x] `has_problem` flag propagates from `MacroBinding` → `MacroDef` → `expand.ml`
- [x] Problem value reaches the macro as first implicit argument
- [x] Macro can match on problem kind with standard ADT matching
- [x] Test: ExprProblem → 1, PatternProblem → 2, DeclProblem → 3
- [x] All 281 tests pass

## Design notes

Problem-aware macros use `match why do | Syntax.ExprProblem -> ...` which works at runtime (NBE `VCon` destructuring). Must be qualified (`Syntax.ExprProblem`, not `ExprProblem`) for pattern elaboration to resolve the constructor against the scrutinee's nominal type.

There is no type-level refinement: all branches must return the same type. Future stages may need flow-sensitive typing or a `type-match` construct that refines the expected return type by problem kind.

## Remaining work

1. **Integration with elaborator**: Automatically set `current_problem` from elaboration context (currently done manually via `~current_problem` in test helper)
2. **Checked expression mode**: `check[why](x)` where `why` distinguishes infer vs check
3. **Problem-dispatching macros**: Macros that return different syntax structures per problem kind
