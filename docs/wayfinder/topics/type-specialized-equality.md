# Type-specialized equality in core_tt

`core_tt` exposes polymorphic `==` and `!=` while keeping primitive equality operations monomorphic internally.

## Shape

User code writes ordinary equality:

```fun
x == y
x != y
```

The user-visible operation is trait-polymorphic through an implicit `Eq` bound:

```fun
(==) : [A : Eq] -> A -> A -> Bool
(!=) : [A : Eq] -> A -> A -> Bool
```

Implicit insertion infers `A` from the operands and resolves an `Eq(A)` implementation.

## Primitive support

The implementation supports primitive equality for core atom types:

- `I64`
- `Bool`
- `Char`
- `Unit`
- `String`

Nominal and record equality are intentionally not automatic structural equality. They require explicit `Eq` implementations.

## Type-head specialization

The equality implementation relies on matching over `Type` values with primitive type-head patterns such as `I64`, `Bool`, `Char`, and `Unit`. `Type` remains open, so matches over `Type` need a fallback branch unless the expected type or surrounding logic proves a narrower domain.

## Operators

The parser treats built-in symbolic operators as fixed syntax for applying known bindings. Equality operators share the normal fixed precedence table and are not locally overrideable operator declarations.

Parenthesized operator identifiers can be used as values:

```fun
do
  same = (==)
  same(1, 1)
end
```

Bare prefix symbolic syntax remains invalid.

## Verification

Equality behavior is covered by the core elaboration and runtime tests:

```sh
dune exec test/semantic/test_elaborate.exe
dune exec test/backend/test_core.exe
```
