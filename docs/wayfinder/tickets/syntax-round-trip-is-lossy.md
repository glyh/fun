---
title: The syntax round trip is lossy
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# The syntax round trip is lossy

## Question

A macro sees syntax through the reflection ADTs and builds syntax from them.
The model requires the **round trip** — destructuring into the ADTs and
reconstructing — to be the identity: a macro that merely reflects a form and
rebuilds it changes nothing. Found (as an inventory) by the
[macro domain-model pass](../topics/core-tt-domain-model-macros.md), which
also decided reflection is **total** — nothing opaque, the `StxExpr` escape
hatch is scaffolding.

Today the round trip loses fields in both directions, and degrades silently
where it should fail loudly.

## Inventory

`Macro_eval` (`wrap` = `Syntax.t` → value, `unwrap` = value → `Syntax.t`):

| field | wrap | unwrap |
|---|---|---|
| id **scope** | erased — `id_to_value` writes `("scope", VAtom (I64 0L))` | hardcoded `Scope_set.empty` — the field is not read |
| `Lam` param / `Let` **type annotations** | preserved | discarded — `type_ = None` always |
| `Ap` **explicitness** | preserved | hardcoded `Explicit` |
| `PatCon` **path** | destroyed — the id is built from the bare constructor string, synthetic span, empty scope | rebuilt as `PatCon ([], name, args)` |
| span **line/col** | preserved | hardcoded `None` (file and bytes round-trip) |

And four silent degradations, where the model demands a loud error:

1. a non-record unwraps to an id named `?` (`value_to_id`);
2. `value_to_bool` maps *any* value that is not `True` to `False` — without
   checking the nominal, so any constructor named `True` matches;
3. `wrap_stx_decl` skips non-`Let` bindings (`| _ :: rest -> go rest`) — a
   macro receiving a decl list containing a type binding sees it vanish;
4. pattern argument lists that fail to unwind are `List.filter_map`-filtered
   rather than rejected.

## Progress (2026-09-14)

The round trip over the reflected forms is now the identity, pinned by
`test_round_trip_is_identity`:
- scope sets travel as an opaque `Scopes` atom (M11);
- annotations, explicitness, span positions and `PatCon` paths are all carried;
- `Id.span` is typed `Option(Span)`, as every other span is (it was declared
  `Span` while reflection wrote an option);
- all four silent degradations now fail the unwrap, which the call sites report
  as errors.

A non-`Let` binding in a decl list now rides as an undecomposed `VStx`.
What remains is **totality**: every form beyond `Var`/`Atom`/`Ap`/`Lam`/`Let`
is still undecomposed. That needs mutually recursive nominal types first.

## Why it matters

- The **scope** row is the soundness slice already ticketed as
  [procedural-macros-capture-use-site-variables](procedural-macros-capture-use-site-variables.md):
  a spliced argument loses its occurrence scope and is captured by the macro's
  binder. This ticket is the general defect that one is a slice of.
- The **annotation** and **explicitness** rows change meaning: a macro that
  reflects `fn(a : I64) -> …` and rebuilds it produces an unannotated lambda —
  elaboration of the output is not elaboration of the input.
- The **`?`** and **`False`** rows misreport: the failure surfaces far from
  its cause, as an unbound name or a silently-wrong flag.
- The port must not transliterate two lossy functions whose names (*wrap*,
  *unwrap*) suggest they are conversions. Under total reflection the ADTs
  *are* the syntax; there is nothing to convert.

## Direction

Grow the reflection ADTs toward totality (one constructor per `Syntax.t`
kind), and make wrap/unwrap preserve every field — scope sets through the
value layer (an id's scope must survive as a value, not as a dummy `I64 0`),
annotations, explicitness, `PatCon` paths, span positions. Replace the four
silent degradations with errors naming the value that failed to reflect. The
scope-set row is fixed by the same change that fixes
[procedural-macros-capture](procedural-macros-capture-use-site-variables.md);
sequence them together.
