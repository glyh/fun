---
title: "Port: the expander's macro runtime interface, and roles through imports"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
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

## Resolution (2026-09-16)

Merged from `port/macro-runtime` (`3681ddc`, `54ed17d`, `178ca23`, merge `107a91c`).
`IMacroRuntime` is declared in `Fun.Expand`, implemented by `Loader`, required by
the expander's constructor (no optional callbacks, no `MissingCallback`). An
`import` loads its unit's syntax where it is written; a unit is expanded once, by
its own expander, and that expansion is reused to elaborate it. A unit's public
roles bind only inside the open or binder that imported it; names a unit's syntax
form introduces can resolve to that unit's members; `OpenSuppliesRole` is
enforced; `export M` re-exports a unit's roles; `stronger_than(M.g)` finds a group
through a unit. `Enforest` is an instance class carrying its `EnforestEnv`; the
thread-static environment is gone. 7 new shared cases agree with the prototype
(`import-open-role-in-region`, `import-binder-role-in-region`,
`order-group-through-unit-path`, `imported-form-names-unit-member`,
`import-open-supplies-unit-role`, `open-supplies-role`,
`open-supplies-role-declared-in-region`); `import-open-role-not-after-region` is
honestly "not ported yet" (the name could be a prelude name). C# 214/658; xUnit 114.

**Scope change:** no separate runtime-free syntax-exports entry point was built.
Units are expanded once through the loader, so nothing needs a pass that expands
without a runtime, and the case the decision guarded against — an expander
constructed with nothing injected — cannot occur. Add the entry point if something
comes to need it.

**Follow-ups:** the interface's macro members (elaborate and apply a macro) join it
with procedural macros; order groups through nested unit paths (`M.N.g`) are "not
ported yet".
