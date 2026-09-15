---
title: Effects follow-ups from the tunneling run
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
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
   family + parameters. Route by the instance.
3. **The E6 escape check covers only a match's result type**: a closure stored in
   a ref declared outside the handler escapes unchecked. Extend the check to every
   way a value leaves the handler's scope (refs' heaps, captured outer bindings).
4. **An unsolved meta row tail counts as open**, so the call tunnels; decide
   whether an unsolved tail at a call is an error or defaults closed.
5. **Wrong error in one E6 shape**: when the other branch returns a pure closure,
   the rejection is `UnhandledEffects`, not `HandledEffectEscapes`.
