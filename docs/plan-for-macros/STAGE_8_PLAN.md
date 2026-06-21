# Stage 8: Problem-Aware Macros

## Goal

Let macros declare their syntactic return kind at definition time, so the elaborator can validate that:
- An `expr` macro is only called in expression position
- A `decl` macro is only called in declaration position
- A `pattern` macro (future) is only called in pattern position

Macros are tagged by kind at definition:

```fun
macro expr twice(x) -> Syntax.ap(Syntax.ap(Syntax.var("+"), x), x)
macro decl value_binding(name, val) -> multi pub name = val end
```

The kind replaces the previous `[why]` implicit parameter design. The macro body knows its context statically — no runtime branching needed.

## Design References

- `IMPLEMENTATION_PLAN.md` Stage 8 defines the exit criteria.
- Stage 7G/7H/7I provide the computed macro and declaration-template infrastructure this builds on.
- The `Syntax.Problem` ADT (`ExprProblem | PatternProblem | DeclProblem`) is replaced by macro-kind tags.

## Non-Goals For Stage 8

- Do not implement type-aware macros (Stage 9).
- Do not implement stuck macros or schedulers (Stage 10).
- Do not add flow-sensitive typing or `type-match` constructs.
- Do not implement pattern macros until pattern ADT is defined.

## Phase 8A: Kind-Tagged Macro Syntax

Status: Not started.

Introduce the `macro <kind> name(...) -> body` syntax where `<kind>` is one of `expr` or `decl`.

### Scope

- [ ] Parse `macro expr name(params) -> body` and `macro decl name(params) -> body`
- [ ] Store `macro_kind : expr | decl` in `MacroBinding` and `MacroDef`
- [ ] Remove `has_problem` field and `Syntax.Problem`-based implicit parameter

### Requirements

- [ ] Macro kinds are checked at definition time (not at call site)
- [ ] `expr` macros can only be called in expression position
- [ ] `decl` macros can only be called in declaration position
- [ ] Existing macro tests continue to work (default to `expr` kind)

## Phase 8B: Elaborator Integration

Status: Not started.

Thread the macro kind through the expander and validate call sites.

### Scope

- [ ] `Expand_ctx` tracks current context kind
- [ ] Macro call expansion validates kind matches expected context
- [ ] Error on mismatched macro kind at call site
- [ ] `parse_expand` sets context kind automatically (expression for `parse_expr`, declaration for `parse_module`)

## Phase 8C: Decl Macros

Status: Not started.

Implement `macro decl` — macros that produce declaration syntax.

### Scope

- [ ] `macro decl foo(name, val) -> multi pub name = val end`
- [ ] Decl macros integrate with Stage 7H/7I declaration-template machinery
- [ ] `multi ... end` output from decl macros

### Tests

- [ ] Decl macro produces a module-level binding
- [ ] Decl macro rejected in expression position
- [ ] Expr macro rejected in declaration position

## Exit Criteria

- [ ] Macro kinds (`expr`/`decl`) are declared at definition time
- [ ] Call sites validate kind against expected context
- [ ] No runtime problem-dispatch in macro bodies
- [ ] All existing tests pass (kind defaults to `expr`)

## Phase 8D (future): Pattern Macros

Deferred until `Syntax.Pattern` ADT is defined. Pattern macros will use `macro pattern` tag similarly.

## Implementation Plan (from IMPLEMENTATION_PLAN.md)

```
Stage 8: Problem-Aware Macros

- Thread problem information through expansion
- Add macro API to inspect the current problem
- Allow a macro binding to expand differently by problem kind
- Integrate with bidirectional elaboration
```

Updated design: replace implicit `[why]` parameter with declarative `macro expr`/`macro decl` syntax.
