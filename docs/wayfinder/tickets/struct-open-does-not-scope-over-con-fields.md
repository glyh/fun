---
title: Struct open does not scope over con_fields
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-15
resolution: Implemented. Struct items are one source-ordered list (a field is a FieldBinding item); a field type sees earlier opens and bindings and leaves as a value; methods are checked after the last field; a field type mentioning an earlier method is FieldTypeMentionsMethod. No dependent fields and no own name, both as unbound names (no dedicated message).
assignee:
blocked_by:
---

# Struct open does not scope over `con_fields`

## Question

In a `struct … end`, an `open` scopes over the later *bindings* but not over the
record field types. Should it, or is the asymmetry the intended rule?

## Evidence

```
M = module pub T = I64 end
R = struct open M; f : T end    (* UnboundVariable "T" *)
S = struct open M; pub m = k end (* works *)
```

`Elab_infer`'s `Struct` case elaborates `con_fields` as a group *before* the
binding fold, so nothing in the binding list — an open included — can affect a
field type. The module form has no such split and behaves as expected.

Found while implementing
[module-level open](module-level-open-strict-imported-modules.md); the current
behaviour is documented on `Surface.OpenBinding` rather than fixed, because the
fix is a change to how structs elaborate, not to the open.

## Sketch of the work

1. Decide the rule. A struct is a record type *and* a namespace; "field types
   see nothing from the body" is defensible, but it makes `open` mean different
   things in `module` and `struct`, against Consistency > Flexibility.
2. If fields should see earlier opens, `con_fields` and `bindings` have to
   elaborate in one source-ordered pass instead of two phases — which is also
   what a field type referring to a type *bound in the same struct* would need,
   so check whether that is wanted at the same time.

## Research (2026-09-13)

Read-only: code reading plus REPL probes (`dune exec fun < probe`). No source
changes.

### Finding 1 — the split is in all three layers, not just the elaborator

| Layer | What happens | Where |
|---|---|---|
| Enforest | `parse_struct_items` sorts each statement into `fields` or `bindings`. **Source order between the two is lost before expansion runs.** | `enforest.ml:1730` |
| Expand | `con_fields` are expanded first, in the struct's outer context. The binding list is expanded afterwards, threading the scopes each binding introduces to later bindings. Field types never receive those scopes. | `expand.ml:419` |
| Elaborate | `con_fields` are elaborated first, as a group, to build the partial struct type that method bodies see as `Self`/`self`. Then the binding fold runs. | `elab_infer.ml`, `Struct` case |

Fixing only the elaborator is impossible, because it no longer knows where the
`open` stood relative to the fields.

### Finding 2 — what a field type can see today

| # | Probe | Result |
|---|---|---|
| 1 | `M = module pub T = I64 end; R = struct open M; f : T end` | `UnboundVariable "T"` (the ticket's case) |
| 2 | `do open M; struct f : T end end` | works; expression-level `open` is the workaround |
| 3 | `R = struct pub T = I64; f : T end` | `UnboundVariable "T"`: fields don't see same-struct bindings either |
| 4 | `R = struct n : Type; v : n end` | `UnboundVariable "n"`: fields don't see earlier fields (no dependent records) |
| 5 | `N = module open M; pub x : T = 4 end` | works: modules follow statement order |
| 6 | `N = module pub x : T = 4; open M end` | `UnboundVariable "T"`: consistent forward-reference rule |
| 7 | `T = Bool; M = module pub T = I64 end; R = struct open M; f : T end; R{f = 1}` | **`CannotUnify(Bool vs I64)`**: the field silently binds the *outer* `T`, not the opened one |
| 8 | `T = I64; M = module pub T = Bool end; R = struct open M; f : T; pub g : T = True end` | accepted: within one struct, `T` means `I64` in the field and `Bool` in the binding |

Probes 7 and 8 matter most. When an outer name exists, the asymmetry doesn't
produce an error. The same written name silently resolves to different types
depending on whether it sits in a field or a binding.

### Finding 3 — the two-phase split is load-bearing for `self`

```
C = struct pub method get() -> self.value; value: I64 end;  C.get(C{value = 4})  → 4
C = struct a: I64; pub method get() -> self.b; b: I64 end;   C.get(C{a=1; b=9})   → 9
```

Methods see **every** field, including fields declared after them. That works
only because all field types are elaborated before any binding. A naive
"one source-ordered pass" would break it: `self`'s type would lack the later
fields.

(Unrelated but observed: `self.value + C.k` inside `C` gives
`UnboundVariable "C"`, while plain `k` works. A struct can't name itself from
its own body.)

### Finding 4 — side discovery: macros escape their block

While probing whether macros reach field types, a macro defined inside a
`struct`, `module` or nested `do`, `pub` or not, turned out to be callable
**after the block ends**. Spun out as
[block-local-macros-leak-by-written-name](block-local-macros-leak-by-written-name.md).
This is also why a macro defined in a struct body appears to reach that struct's
field types in either order. It is not evidence that fields see bindings.

### The design space

The real question is not "should `open` reach fields" but **what a field
declaration is**. Two readings fit the evidence:

- **(A) Fields are a signature in the outer scope.** Keep today's behaviour and
  make it the stated rule: field types see only what's outside the struct.
  Consistency cost: `open` means something different in `struct` and in
  `module`, and probes 7 and 8 stay silently confusing. At minimum, an `open`
  (or type binding) that textually precedes a field should then be an error or
  warning, not ignored.
- **(B) Scope in source order, `self` over all fields.** Each field type sees
  the preceding opens and non-method bindings. Method bodies are checked after
  every field is known, as class members are in Scala, Kotlin and C#, or as
  OCaml `module rec` checks against the full signature. This needs:
  1. an interleaved item list out of the enforester (e.g. a field as a
     `struct_binding` variant, which triggers the field-propagation checklist in
     `CLAUDE.md`);
  2. expander scope threading that includes field types;
  3. an elaborator that walks items in order, **defers method bodies** until all
     field types exist, and rejects a field type that depends on a method
     (otherwise it's a cycle: field → method → `self` → all fields).

  This matches `module` statement order (Consistency > Flexibility) and fixes
  probes 1, 3, 7 and 8. It also opens the door to probe 4 (dependent fields)
  later, but doesn't require it.

A third option, letting only `open` reach fields as a special case, fixes probe 1
but not 3, 7 or 8, and adds a rule of its own. Not recommended.

**Recommendation: (B)**, with deferred method bodies. It's the only reading
under which one written name means one thing throughout a struct.

Interactions to carry into grilling:
- Record declarations (`type R A = {…}`) elaborate through the same `Struct`
  case with `ctx.self_type = VSelfType`, and
  [recursive-records-cannot-hold-a-record](recursive-records-cannot-hold-a-record.md)
  will rework that path. Settle this ticket first, so the struct elaborator is
  rewritten once.
- Whether dependent fields (probe 4) are wanted. Under (B) they become a
  telescope over fields. That's a separate decision, but it shouldn't be closed
  off by accident.

## Resolution

_Unresolved._ The rule is a grilling decision between (A) and (B), with (B)
recommended.

## Grilled (2026-09-15): source order, deferred method bodies

1. **(B) Source order.** A field's type sees every `open` and binding written
   before it, as in `module`. Method bodies are checked after all fields, so a
   method sees every field, including later ones. A field type that depends on a
   method is a cycle error.
   ```fun
   R = struct {
     open M;
     f : T;                      // T from M
     pub method get() { self.g } // sees g, declared later
     g : I64
   }
   ```
2. **No dependent fields for now.** A field type may not mention another field
   (`struct { n : Type; v : n }` is an error). Dependent records would be their
   own ticket.
3. **A struct does not see its own name.** `C` binds after `C = struct { … }`,
   as any binding does; the body uses bare names and `self`. `C.k` inside `C`
   is an error that suggests `k`.

## Implemented (2026-09-15)

- `Syntax.Struct` carries only `bindings`; a field is `FieldBinding { name; type_ }`
  (reflected `DeclField(String, Expr)`, `RawStruct(Option(Span), List(Decl))`).
  The enforester keeps source order, and expansion threads each item's scopes to
  the next, fields included.
- The elaborator walks items in order. A field's type is elaborated where it
  stands and quoted at the struct's own level (every slot an item adds holds a
  value), so `Core.Struct` and the evaluator are unchanged. Methods written
  before the last field are elaborated right after it.
- Choice this run made: `Self` in an item other than a field or method (a
  `pub id = fn(b : Self) …`, an `impl Eq(Self)`) is the fields written so far,
  where it used to be every field.
- Not done: dedicated messages for decisions 2 and 3. Both are unbound names
  today. A message for `C.k` inside `C` needs to know that no open supplies `C`,
  which only elaboration knows (the prelude is open almost everywhere), so the
  expander cannot report it by scope set alone; doing it at elaboration would
  mean matching the binder by spelling or carrying the struct's binder in the
  open choice.
