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
   (or at generalisation) asks for an annotation: "can't infer the effects of `g`;
   write `can {…}` or `can {}`". No default.
   **Routing part resolved by the item 2 design**: routing is decided at run time
   by instance, so an unsolved tail no longer changes which handler catches a
   request. **Generalisation part and `can any`: not implemented — open
   questions** (see below).
5. **Wrong error in one E6 shape**: when the other branch returns a pure closure,
   the rejection is `UnhandledEffects`, not `HandledEffectEscapes`.
   **Fixed 2026-09-15**: an `UnhandledEffects` raised while a match's branches
   elaborate that names an instance the match handles is reported as
   `HandledEffectEscapes`.
   **Superseded (2026-09-16):** `can any` / `->{any}` was dropped; see effect-arrow-syntax.
## Open questions from the effects-followups run (2026-09-15)

- **An unsolved `can _` in a let-bound lambda's parameter.** Making an unsolved
  row tail an error at generalisation rejects two existing tests:
  ```fun
  Callback = Unit ~> I64;
  app = fn(g : Callback) { g() };          // g's row: nothing determines _
  f : Unit ~> I64 = fn(_) { perform State.get () };
  ```
  Should `_` there be generalised into an implicit row parameter (effect
  polymorphism, `app : [r] -> (Unit -> I64 can {| r}) -> I64 can {| r}`), or be
  the decided error? Also: `Callback = Unit ~> I64` evaluates its meta once, so
  every use of `Callback` shares one row.
- **What `can any` means at a call.** `f : Unit -> I64 can any` — may it be called
  where the caller's row is closed (then the call performs "anything" and the
  caller must be `can any` too), and does a handler discharge any part of it?
