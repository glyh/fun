# Stage 8: Problem-Aware Macros

## Goal

Let macros declare their syntactic return kind, so the elaborator can validate that macros are used in the correct context. The kind is determined by the macro's **return type** — either via inference (best effort) or explicit annotation.

```fun
macro twice(x) -> Syntax.ap(Syntax.ap(Syntax.var("+"), x), x)   -- inferred: Expr
macro bind(x) : Decl -> multi pub x = 0 end                      -- annotated: Decl
```

This follows the same convention as regular functions: return type determines usage. No `macro expr` / `macro decl` syntax tags — the kind comes from what the macro produces.

## Design References

- `IMPLEMENTATION_PLAN.md` Stage 8 defines the exit criteria.
- Stage 7G provides the `Syntax.Expr` ADT this builds on.
- The `Syntax.Problem` ADT and `[why]` implicit parameter prototypes are deprecated.

## Key Design Decisions

### Kind inference

Like function return types, macro kinds are inferred from the body:
- Body returns a `Syntax.Expr` node → `Expr` kind
- Body returns a declaration structure → `Decl` kind
- Best-effort inference; annotation (`: Decl`) when ambiguous

### Name shadowing

Later bindings shadow previous regardless of kind. A name cannot be used for both an `Expr` macro and a `Decl` macro simultaneously — the later binding wins.

### No implicit problem parameter

The `[why]` implicit parameter is removed. The macro's kind is static, not a runtime value to branch on.

## Phase 8A: Kind-Tagged Macro Syntax

Status: Not started.

### Scope

- [ ] Parse optional `: Decl` annotation on macro definitions
- [ ] Infer kind from body when annotation is absent
- [ ] Store kind in `MacroBinding` / `MacroDef`

## Phase 8B: Elaborator Integration

Status: Not started.

### Scope

- [ ] `Expand_ctx` tracks current context kind
- [ ] Macro call validates kind matches expected context
- [ ] Error on mismatched kind at call site
- [ ] `parse_expr` sets expr context, `parse_module` sets decl context

## Exit Criteria

- [ ] Macro kinds inferred from return type or explicit annotation
- [ ] Call sites validate kind against expected context
- [ ] Name shadowing works regardless of kind
- [ ] All existing tests pass (macros default to `Expr` kind)
