---
title: Handlers tunnel callback effects
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Handlers tunnel callback effects

## Decision

Handling is lexical, not dynamic (Zhang & Myers's *tunneling*). A handler
handles the effects its own scrutinee performs; an effect raised by a function
passed in belongs to that function's effect row and tunnels past handlers in
code that is polymorphic over that row. Vocabulary: **Handler**, **Accidental
handling** in [`CONTEXT.md`](../../../CONTEXT.md).

## Evidence — accidental handling today

```fun
effect Exc = sig raise : I64 -> I64 end

find = fn(pred : I64 -> I64, x : I64) ->
  match (do v = pred(x); if v > 3 do perform Exc.raise(v) else v end end)
  do v -> v | effect Exc.raise n -> 0 end

user = fn(x : I64) -> perform Exc.raise(x)

match find(user, 1) do v -> v | effect Exc.raise n -> 999 end
```

Answers `0` in the REPL today: `find`'s internal handler catches the user's
raise, though nothing in `find`'s type mentions `Exc`. Under tunneling it
answers `999`.

## Why

- Effect rows are visible in signatures
  ([bare-arrow-is-pure](bare-arrow-is-pure.md)); tunneling makes the runtime
  respect what the row on `pred` already says about whose effects are whose.
- Parametricity: a function polymorphic in a row cannot observe the effects in
  it, just as a function polymorphic in `A` cannot inspect an `A`.
- The dynamic alternative (Koka, OCaml 5) needs private effects or an explicit
  `mask` at every higher-order boundary, and fails silently when forgotten.

## Cost

Handlers become lexically scoped capabilities: an operation is routed to the
handler its row was bound to (evidence passing), not to the nearest enclosing
one. Lexa (OOPSLA 2024) and zero-overhead lexical handlers show
this compiles without a runtime search — relevant for C FFI.

Interacts with the deep, one-shot, `resume`-through-lambdas choices in
[algebraic-effects](../topics/algebraic-effects.md); each needs re-checking
under lexical routing.

## Sources

- [Abstraction-Safe Effect Handlers via Tunneling (POPL 2019)](https://cs.uwaterloo.ca/~yizhou/papers/abseff-popl2019.pdf)
- [Lexical Effect Handlers, Directly (OOPSLA 2024)](https://cs.uwaterloo.ca/~yizhou/papers/lexa-oopsla2024.pdf)
- [Zero-Overhead Lexical Effect Handlers](https://doi.org/10.1145/3763177)
- [A Type System for Effect Handlers and Dynamic Labels](https://link.springer.com/chapter/10.1007/978-3-031-30044-8_9)

## What may outlive a handler

Decided with the tunneling rule; vocabulary **Handler scope**.

Allowed — a continuation saved and resumed later (schedulers, async). Resuming
re-enters the handler; one-shot is still checked at run time.

```fun
match task(()) do
  v -> v
  | effect Async.pause _ -> push(queue, fn(u) -> resume(()))
end
pop(queue)(())
```

Rejected at compile time — a closure whose row names an effect the handler
handles, escaping that handler:

```fun
leak = match 0 do
  v -> fn(u) -> perform Log.say("hi")
  | effect Log.say s -> resume(())
end
-- error: `Log` is handled on line 3, but escapes it in the type of `leak`
```

The handler binds its effect like a type variable; the check is the same
escape check existentials and `runST`-style brands need. Rejected alternatives:
Effekt's second-class functions (too restrictive where everything is
first-class) and a run-time error on the late call (silent until run).
