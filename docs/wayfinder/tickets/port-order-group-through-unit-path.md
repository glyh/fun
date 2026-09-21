---
title: "Port: an order group named through a unit member's path"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: an order group named through a unit member's path

A **real gap** found by [the parity conversions](port-parity-conversions.md) fork on
2026-09-20, and **verified by the integrator**: the prototype answers `1`, the port
throws

```
not ported yet: an order group named through a unit member's path
```

(`dotnet/src/Fun.Expand/Enforest.Roles.cs:343`). Reproduce with two units — `m`
declaring the group, `wrapper` re-exporting the module:

```fun
-- deep.unit-m.fun
pub order g;

-- deep.unit-wrapper.fun
pub M = import "m";

-- deep.fun, expect 1
{ W = import "wrapper"; infix (@@) W.M.g ($x, $y) { $x }; 1 @@ 2 }
```

The path is `W.M.g` — depth > 1 through a module member. **The audit classified this
row as parity and was wrong**: it read the site as the "dotted group reference" open
item on [brackets-decide-grouping](brackets-decide-grouping.md), but the prototype
does resolve a dotted path of depth > 1 here; what remains open on that ticket is a
different, narrower case. So this is a gap to port, not a refusal to reword, and the
audit's parity table needs the correction (done — see that ticket).

## Fix

Small and self-contained: `ResolveOrder` needs the same dotted fold as
`unit_path_of`-style resolution elsewhere, so a path through a module member is
followed to its final member before the role is looked up. Follow
[convention 7](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16) if the work
wants its own partial file; check `Enforest.Roles.cs`'s existing path handling first —
the depth-1 case already works, so the difference is the fold, not the lookup.

Compare the prototype's role resolution in `lib/expand/enforest.ml` (`is_poly_arrow`,
`add_id_scope_if`, the role lookup in `enforest_util.ml`) — and note the prototype
binds `~>` and the prelude operators as roles in `enforest_util.ml:55`, so the depth-1
path is exercised by every operator.

## Tests

The three files above are the shared case (`expect` `1`), and the prototype passes it,
so it is an **ordinary case, not a divergence**. Add the companion shapes while you are
there, each verified in both runners first: the same path used through a *binder* rather
than an `import` (`W = module { … }`), and a depth-2 path that must still fail when the
named member is not an order group (add that one only if the prototype agrees it errors).
