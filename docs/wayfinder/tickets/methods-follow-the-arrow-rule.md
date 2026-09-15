---
title: Methods follow the arrow rule — pure unless they say `can`
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
decided: 2026-09-15
assignee:
blocked_by:
---

# Methods follow the arrow rule — pure unless they say `can`

## Defect

A method's type carries no effect row, so what a method body performs is dropped
(noted by the one-pass-effects run with a `ponytail:` comment). A method that
performs passes the checker and fails at run time.

```fun
effect Log = sig { write : String -> Unit };
Counter = struct {
  n : I64;
  pub method bump() { perform Log.write("bump"); self.n + 1 }
};
// c.bump() at the top: accepted today, crashes at run time
```

## Decision (grilled 2026-09-15)

A method is a function stored in a struct, so it follows the bare-arrow rule
([bare-arrow-is-pure](bare-arrow-is-pure.md)): pure unless it declares a row.

```fun
pub method bump() can {Log} { perform Log.write("bump"); self.n + 1 }   // ok
pub method bump() { perform Log.write("bump"); … }                       // error: Log not declared
pub method any() can _ { … }                                             // row inferred
```

Trait method signatures carry rows the same way; an impl's method must fit its
trait signature's row. Remove the `ponytail:` drop in the elaborator.
