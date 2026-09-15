---
title: A let-bound signature cannot be a parameter type; dependent signatures fail
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# A let-bound signature cannot be a parameter type; dependent signatures fail

Found by the decl-output-open-param run (2026-09-15); pre-existing.

```fun
Sig = sig { x : I64 };
f = fn(m : Sig) { m.x }          // CannotUnify(module value vs Type)
g = fn(m : sig { x : I64 }) { m.x }   // works (inline)

h = fn(m : sig { T : Type; v : T }) { m.v }   // fails, even without open
```

- A signature is a value (types are values), so a let-bound one must work
  wherever the inline form does.
- A dependent signature (a later member's type mentions an earlier type member)
  must elaborate.
- Also: opening a parameter whose signature contains an anonymous impl is
  `NotAModule` (the impl has no name to project).
