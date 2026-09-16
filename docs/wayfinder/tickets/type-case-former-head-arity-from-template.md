---
title: A type-case head that is a type former takes its arity from the template
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# A type-case head that is a type former takes its arity from the template

Found by the C# port's patterns fork (2026-09-16). **Fixed in the C# port only**;
the OCaml prototype keeps the defect.

## Defect

```
({ Opt = fn(A : Type) { enum { Some(A), None } };
   classify : Type -> I64 = fn(T) { match (T) { Opt(I64) => 1, Opt(x) => match (x) { Char => 2, _ => 3 }, _ => 0 } };
   (classify(Opt(I64)), classify(Opt(Char)), classify(Opt(String)), classify(I64)) }).1
```

fails in the prototype with `ElabError(PatternArityMismatch)`: the nominal head's
arity is read from the declaration's `num_params`, which is 0 for an enum whose
parameters come from an enclosing `fn(A : Type)` former. The closed
[pattern-head-accepts-type-formers](pattern-head-accepts-type-formers.md) decides a
former is a valid type-case head, matched on its parameters, so this gives 2.

## Conformance

`values/type-case-former-parameter` (2), listed in
`test/conformance/prototype-divergences.txt`.
