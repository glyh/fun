# There is no subtyping in `quill`

`quill` has no subtyping relation. NbE convertibility is the only equality, records are
structural and exact, and there is no coercion term anywhere in the implementation.
The one place width is admitted is module↔signature unification, and it is a property
of signatures, not a general relation between types.

## The decision

- **Convertibility is the only equality.** Two types are the same when they quote to the
  same term (`Nbe.Rec.cs:115`). There is no subsumption check and no coercion:
  `grep -rniE 'coerc' src/` = **0** hits.
- **Checking is conversion-only.** `Elaborator.Check` (`Elaborator.cs:342`) falls through
  to `AgreeWithExpected` (`Elaborator.Structs.cs:331`) → `Context.Unify`
  (`Elaborator.cs:100`) → `Unify.Values` (`Unify.cs:11`). Nothing on that path accepts a
  value of one type where another is expected without the two being convertible.
- **Records are structural and exact.** A record type with more fields does not unify
  with one having fewer: probing `Q{x=1;y=2}` against `struct{x:I64}` gives
  `structs with different members`.
- **References are unrelated across heaps.** `Ref(h, A)` and `Ref(h', A)` are different
  types related by nothing — `Unify.cs:84` requires the heaps themselves to be
  convertible, and `Unify.cs:85` requires the same cell. This is what heap brands do
  *without* a subtyping rule: a mutable reference is not a subtype of a shared one,
  it is a different type.

## The one exception: module↔signature width

`Unify.Structs.Modules` (`Unify.Structs.cs:17`) lets a **partial** side — a signature's
instance — need only its own members present in the other side. The rule is
one-directional and adds no coercion:

```
f = fn(m : sig { x : I64 }) { m.x };
f(module { pub x = 42; pub y = True })   →  VALUE 42

f = fn(m : sig { x : I64; y : Bool }) { m.x };
f(module { pub x = 42 })                  →  error: no member `y`
```

Extra members are tolerated; missing members are refused. It is pinned by
`test/conformance/cases/values/core-021.qll` and was already documented as
"module-type unification (width subtyping)" in
[`port-structs-records-signatures.md`](../tickets/port-structs-records-signatures.md).

`grep -rniE 'subtyp' src/` = **3** hits, all comments naming this rule:
`Core.Structs.cs:8`, `Unify.Structs.cs:17`, `Elaborator.Traits.cs:592`.

## Why the record exists

The Graydon-constraint audit (2026-10-01) found zero mentions of subtyping across the
design map, `docs/STATUS.md`, and `docs/wayfinder/topics/` — the one row of Graydon's
ten constraints with no record at all. This is that record. The premise it was filed
under ("no subtype rule exists anywhere") turned out to be false in exactly the one
place above, which is why the exception is named rather than smoothed over.
