# Type-Aware Macro Interleaving

> **Status note**: This is a design document describing the required fix for the
> Stage 10 annotation-name disambiguation limitation (expand/elaborate interleaving).
> The fix is not yet implemented. See **[STATUS.md](STATUS.md)** for current
> macro completion status.

## Problem

Historically, macro annotations such as `: Expr(A)` were resolved in
`enforest.ml` with a static `known_type_names` list. If `A` was in that list, it
became a type constraint. Otherwise, an uppercase name became an implicit macro
binder of type `Syntax.R`.

The current migration state has removed that static list: leading-uppercase
annotation names uniformly become binders until the semantic driver can resolve
constraints against the current prior type namespace.

That is wrong for user-defined, aliased, imported, or re-exported types. The
decision should be semantic: `Expr(A)` is a constraint if `A` resolves as a type
or type-level value in the elaboration context, and a binder only otherwise.

## Why A Local Fix Is Not Enough

This decision changes macro arity. A binder adds an implicit parameter to the
macro function before the macro is compiled. A constraint does not. Therefore,
the parser cannot decide correctly, and a post-expansion elaborator check is too
late.

Parser-only or expander-only name sets are also insufficient. They miss value
aliases, imported module aliases, and future forms where a name resolves to a
type-level value without being syntactically declared as a type in the same
parser scope.

The Klister notes under `docs/plan-for-macros/extracted/klister/commentary/`
describe the intended architecture: macro expansion and typechecking are
interleaved. Expansion can block on type/elaboration facts, and typechecking can
block on expansion results.

## Required Compiler Shape

The current pipeline is:

```text
source -> Enforest -> Expand -> Lower -> Elaborate -> Core/NBE
```

The required shape is either:

```text
source -> Enforest -> sequential expand/elaborate module body -> Core/NBE
```

or, long term:

```text
source -> Enforest -> task queue of expansion and typechecking problems -> Core/NBE
```

The minimal useful step is a sequential module-body handshake. When processing a
module, prior bindings must be elaborated before compiling a later macro
definition. Then the macro annotation resolver can ask the elaboration context
whether `A` is known as a type/value before deciding macro arity.

## Seams To Replace

- `Parse_expand.expand_lower_syntax` currently runs full expansion before
  elaboration.
- `Expand.expand` currently compiles `MacroBinding` via an `elaborate` callback
  whose context only contains builtins and already-opened stdlib bindings.
- `Expand.expand_struct_bindings` scope-expands prior type/value bindings but
  does not elaborate them before compiling macros.
- `Elab_infer` currently sees lowered `Surface.t` after macro bindings have been
  removed from runtime output.
- `Core_loader.visit_macros` compiles imported macros through the same
  pre-elaboration path.

## Design Target

1. Preserve ambiguous annotation syntax through parsing.
2. Resolve ambiguous macro annotations in an elaboration context before macro
   compilation.
3. If the name resolves as a type/value, compile the macro as constraint-only.
4. If the name is unresolved, compile the macro with an implicit `Syntax.R`
   binder.
5. Use resolved values or nominal ids for constraint checks, not string-name
   equality.
6. Apply the same mechanism to imported macros and module aliases.

## Regression Tests To Add Later

- `type MyTag = ...; macro m(_) : Expr(MyTag) -> ...` treats `MyTag` as a
  constraint, not a binder.
- `MyInt = I64; macro m(_) : Expr(MyInt) -> ...` constrains by the aliased
  builtin type instead of creating an implicit `MyInt : Syntax.R` binder.
- `Alias = SomeModule; macro m(_) : Expr(Alias.T) -> ...` constrains by the
  aliased type value.
- Imported macro modules compile annotations using the importing/visiting phase
  context correctly.
- Existing type-aware binder macros using unresolved `Expr(A)` still compile and
  receive the expected `Syntax.R` argument.
