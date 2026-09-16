---
title: Checking a lambda against a function type ignores its written parameter type
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Checking a lambda against a function type ignores its written parameter type

Found while porting the elaborator to C# (2026-09-16). **Fixed in the C# port
only** (`dotnet/src/Fun.Compiler/Elaborator.cs`); the OCaml prototype keeps the
defect.

## Defect

`Elab_check.check`'s `Lam (param, body), VPi { domain; … }` case binds the
parameter at the expected `domain` and never reads `param.type_`. A written
parameter type is therefore meaningless in checking mode, though it is honoured
in inference mode (`Elab_apply.infer_lam`), so the same lambda means different
things depending on whether a type is expected.

## Reproduction

```
(fn(x : Char) { x } : I64 -> I64)(1)
```

elaborates and runs to `1` in the prototype. The parameter is declared `Char` and
used as an `I64`.

## Resolution in the port

A written parameter type is elaborated as a type and unified with the expected
domain, exactly as any other annotation is checked, before the body is checked.
The lambda binds its parameter at the expected domain (now known equal).

## Conformance

- `elaborate/lambda-param-type-mismatch` (`error`) is the reproduction above;
  `values/lambda-param-type-agrees` (`1`) is the agreeing case.
- `elaborate/elab-049` expected `ok` for
  `((fn(T : Type, x : T) { x }) : Type -> I64 -> I64)(I64)(42)` - a second
  parameter written `x : T` checked against a domain of `I64` - and passed only
  because of this defect. Its `.expect` is now `error`.

Both `error` cases are listed in `test/conformance/prototype-divergences.txt`, so
the prototype's run expects them to fail.
