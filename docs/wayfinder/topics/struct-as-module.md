# Modules, structs, and module signatures

`module ... end` and `struct ... end` are separate constructs.

- `module ... end` builds a namespace value.
- `struct ... end` builds a structural record type descriptor.

They share member syntax for ordinary bindings, but they are not interchangeable at the type level. A module is not a record type, and a record struct is not a module signature.

## Modules as namespaces

A module can contain private and public bindings:

```fun
do
  Math = module
    helper = fn(x) -> x + x
    pub double = helper
  end
  Math.double(21)
end
```

Only public members are visible through field access or `open` outside the module boundary. Private bindings are available to later members in the same module.

Modules may contain public or private value, type, effect, trait, and impl bindings according to the supported binding forms. They cannot contain record field declarations or methods, and module bodies do not have `self` or `Self`.

## Record structs

Field declarations make a `struct` usable as a record type:

```fun
do
  Point = struct
    x: I64;
    y: I64;
  end
  Point{x = 3; y = 4}
end
```

The struct value is the record type descriptor. `Point{...}` constructs a record instance whose fields are checked against the struct's declared fields.

Record instance fields are accessed with dot syntax:

```fun
do
  p = Point{x = 3; y = 4}
  p.x + p.y
end
```

## Public types and constructors

Modules can contain nested nominal type bindings. Public type bindings expose the type and its constructors as public module members:

```fun
do
  Colors = module
    pub type Color = Red | Green | Blue
  end
  match Colors.Red do
| Colors.Red -> 1
| Colors.Green -> 2
| Colors.Blue -> 3
end
end
```

Expression-side qualified access is ordinary field access. Pattern-side qualified constructor and record paths are resolved during elaboration.

## Open and export

`open M` brings public members of module `M` into scope for subsequent expressions in the enclosing `do ... end` block.

Inside a module, `open M` brings members into scope for subsequent members without re-exporting them. `export M` also includes those public members in the enclosing module's public interface.

`open` is module-only. Record structs are not opened as namespaces.

## Methods, self, and Self

Record-like structs can define methods that use `self` as the receiver and `Self` as the enclosing record type after field declarations. This keeps methods attached to the structural record model.

Modules cannot define methods and cannot refer to `self` or `Self`.

## Module signatures

A signature is its own kind of value, written `sig { … }`; a module is never a
type (`fn(m : module { … })` is `NotASignature`). A signature lists public
requirements; extra public members on the argument are allowed, and a private
binding does not satisfy a requirement.

```fun
get_x = fn(m : sig { x : I64; y : Bool }) { m.x };
get_x(module { pub x = 1; pub y = True })
```

A signature is a **telescope** over the module it describes: a later member's
type may mention an earlier member, read through the module. For a parameter the
earlier member stays abstract; for an argument it is the argument's own member.

```fun
Stack = sig { T : Type; empty : T; size : T -> I64 };
count = fn(s : Stack) { s.size(s.empty) };        // s.empty : s.T
count(module { pub T = I64; pub empty = 7; pub size = fn(x : I64) { x + 1 } })
```

An impl a signature requires is **named** (`eq_T : impl Eq(T)`); an anonymous
`impl` in a `sig` is a parse error. The module must provide a public impl of
that name and type; the parameter reaches it as `s.eq_T`, and `open s` brings it
into trait resolution.

```fun
Ordered = sig { T : Type; eq_T : impl Eq(T) };
same = fn(s : Ordered, a : s.T, b : s.T) { open s; Eq.eq(a, b) };
```

Representation: `Core.Sig body`, where `body` is a `Module { signature = true }`
under one binder (the described module); it evaluates to `VSig` (a closure).
`Nbe.module_type_of` instantiates it with a module value, giving the member
types as a `VModule { partial = true }`. Two signatures are compared under one
fresh module; `s1.T` and `s2.T` of two parameters are distinct.

Unannotated field access remains record-oriented for unknown receivers. Passing modules through functions requires an explicit module signature when the function accesses module members:

```fun
do
  S = module pub x = 42 end
  (fn(s : module pub x = I64 end) -> s.x)(S)
end
```
