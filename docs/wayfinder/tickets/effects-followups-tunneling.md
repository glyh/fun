---
title: Effects follow-ups from the tunneling run
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-16
resolution: All five items done. Item 4's routing half fell out of instance routing; a written ->{_} nothing solves is UnsolvedEffectRow at the end of an entry, naming ->{…} / -> / ~>. The E6 escape check was re-checked after instance routing and extended to module entries and impl dictionary types.
assignee:
blocked_by:
---

# Effects follow-ups from the tunneling run (2026-09-15)

1. **`resume` does not re-enter the value branch (deep handlers, E8).**
   `match (perform E.op(1)) { x => x + 5, effect E.op n => resume(n + 40) }`
   answers 41; a deep handler answers 46 (the resumed computation's result goes
   through the value branch). Pre-existing. **Fixed 2026-09-15** (branch
   `resume-value-branch`): the continuation handed to an effect branch now
   re-enters the handler (`resume_with`), so its result passes through the value
   branch. The shallow-style state test (re-installing the handler per call)
   was migrated to the deep encoding (the handler returns a function of the state).
2. **Tunneling routes by effect family, not instance.** `State(I64)` and
   `State(Bool)` count as one family for the hop count, while E1 says an effect is
   family + parameters. Route by the instance. **Fixed 2026-09-15** (branch
   `effects-followups`): a call with an open row is `Tunnel { named; handlers }`
   - the instances its row names and the lexical handlers enclosing it in its
   function body. At run time a request whose instance is not named (compared
   by `runtime_value_equal`, family and parameters) passes those handlers
   (`effect_request.skips`, replacing the hop count); each `EffectBranch`
   carries its handler's id. No per-family counting remains.
3. **The E6 escape check covers only a match's result type**: a closure stored in
   a ref declared outside the handler escapes unchecked. Extend the check to every
   way a value leaves the handler's scope (refs' heaps, captured outer bindings).
   **Fixed 2026-09-15**: a store into a reference whose heap is not local to the
   match (`effect_sink.stored`, `local_heaps`) is checked like the result type
   (`Elab_match.escape_guard`). Outer bindings cannot be reassigned except
   through references, so references cover it.
4. **An unsolved meta row tail counts as open**, so the call tunnels. **Grilled
   2026-09-15: an error** — an effect row still unsolved where it decides routing
   (or at generalisation) asks for an annotation. No default.
   **Routing part resolved by the item 2 design**: routing is decided at run time
   by instance, so an unsolved tail no longer changes which handler catches a
   request. **Done 2026-09-16**: a written `->{_}` that nothing solved by the end
   of an entry (an expression or a unit's bindings) is `UnsolvedEffectRow`,
   naming what to write instead (`->{…}`, a pure `->`, or `~>`); the check scans
   the rows that entry created (`Elab_effects.require_handled_at_entry`). A pure
   body still solves a written `_` to empty, and a `~>`-minted variable is a
   binder, not an unsolved row.
5. **Wrong error in one E6 shape**: when the other branch returns a pure closure,
   the rejection is `UnhandledEffects`, not `HandledEffectEscapes`.
   **Fixed 2026-09-15**: an `UnhandledEffects` raised while a match's branches
   elaborate that names an instance the match handles is reported as
   `HandledEffectEscapes`.
   **Extended 2026-09-16**: enforcing "`: T` is a pure result"
   ([effect-arrow-syntax](effect-arrow-syntax.md)) made the same shape raise
   `EffectsInPureResult` instead, which slipped past that translation (`main` was
   red on "an escaping closure called at the top is an error"). Both errors are
   now translated.
   **Superseded (2026-09-16):** `can any` / `->{any}` was dropped; see effect-arrow-syntax.

## Closed questions (2026-09-16)

- **An unsolved row in a let-bound lambda's parameter** — answered by
  [effect-arrow-syntax](effect-arrow-syntax.md): `~>` mints a rank-1 row variable
  bound at the enclosing definition, so `app = fn(g : Callback) ~> I64 { g() }`
  is polymorphic rather than unsolved, and only a written `->{_}` nothing
  determines is the error.
- **What `can any` means at a call** — `can any` / `->{any}` was dropped.

## Escape check, re-checked after tunneling by instance (2026-09-16)

`Elab_match.escape_guard` walks a match's result type and anything stored into a
non-local reference. It missed **module entries**: a match returning a module
whose member's row named a handled effect passed (`VStruct` was walked,
`VModule` was not). Fixed in `eb5e072`; impl dictionary types are walked in both,
so the traversal is total over entry kinds. Remaining shape, not reachable today:
a result type that is a `VSig` closure is not opened (it needs the module value),
and a trait impl cannot widen its row past its trait's signature, so no test
exposes either.
