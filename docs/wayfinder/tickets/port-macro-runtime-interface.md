---
title: "Port: the expander's macro runtime interface, and roles through imports"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: the expander's macro runtime interface, and roles through imports

Wave 3 fork. Follow the porting conventions in
[port-core-tt-to-dotnet](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16).

## Scope

- **`IMacroRuntime`**, as decided in the port ticket ("the expander's callbacks
  become one injected capability"): one interface declared in `Fun.Expand`,
  implemented in `Fun.Compiler`, taken **non-nullable** by the expander's
  constructor. It carries what the expander needs from the other side of the
  project boundary — elaborating and evaluating a term, applying a macro value
  under the budget, and loading a unit's syntax (roles and, later, macros). There
  is no `MissingCallback` and no optional field.
- **The runtime-free pass** (a unit's syntax exports, read without elaborating
  anything) is its own entry point, not an expander with nothing injected.
- **Roles through `open (import "u")` and module-level opens of units**, the gap
  the roles fork left (`Loader` lives in `Fun.Compiler`); `OpenSuppliesRole`;
  group paths through a unit (`Std.additive`, 1 case).
- **Retire the roles fork's ambient reading environment** (thread-static, marked
  `ponytail:`) in favour of state the expander carries.
- Procedural `macro` definitions and calls are **out of scope** — the next ticket
  builds them on this interface — but shape the interface so they fit.

## Decided rules to read first

The port ticket's "Decided (2026-09-16): the expander's callbacks become one
injected capability"; domain model I4c, I4e ("the elaborator's expander handle is
a capability, not a context"), I5 (a unit elaborates against the base context);
`docs/wayfinder/topics/core-tt-domain-model-macros.md`;
[expander-handle-is-a-capability-not-a-context](expander-handle-is-a-capability-not-a-context.md).
