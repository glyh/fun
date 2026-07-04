# Private Type Visibility

## Status

Separate feature/design task. Do not implement as part of the current concrete
bug-fix pass.

## Decision

Use the OCaml/SML model. Private types may leak through public bindings, but
outside the defining module they become abstract. Values can be passed around,
but constructors are not available unless the type is public and concrete.

## Surface Syntax

Modules have two mutually exclusive visibility modes:

- Inline `pub` annotations, where public types are concrete.
- A `sig`, where exported types may be concrete or opaque.

Opaque types are declared without a right-hand side in a signature:

```fun
-- Mode A: inline pub
module M = do
  type Hidden = Wrap
  pub type R = RExpr(T) | RDecls | RPat
  pub value = Wrap(1)
end

-- Mode B: sig
sig M = sig
  type R = RExpr(T) | RDecls | RPat
  type Handle
  value : I64
end

module M : M = do
  type Hidden = Wrap
  type R = RExpr(T) | RDecls | RPat
  type Handle = ...
  value = Wrap(1)
end
```

When a signature is present, everything in the implementation body is private by
default and inline `pub` annotations are a compile error. Elevating inline `pub`
to a signature should be mechanical: copy public entries into a `sig`, replace
`=` with `:` for values, and remove `pub` from the body.

## Access Control Model

`open_module_value` should become the single access-control gate. It registers
public type names and public constructors recursively for public submodules.
Private types never register constructors outside the defining module, so
constructors are naturally unreachable.

With this model, `unqualified_constructor_in_scope` should become unnecessary:
the name table itself is the visibility check.

## Cross-Language Consensus

The desired rule matches the surveyed OCaml/SML-style behavior: private
constructors are not usable unqualified after `open` or `import`.
