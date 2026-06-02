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

A module signature is written as a module whose public requirements store member types:

```fun
module pub x = I64; pub y = Bool end
```

In a type position, this is interpreted as a partial module type requiring public members `x : I64` and `y : Bool`. Extra public members are allowed on the concrete module argument.

```fun
do
  get_x : (module pub x = I64 end) -> I64 = fn(m) -> m.x
  get_x(module pub x = 1; pub y = true end)
end
```

The `sig ... end` form is sugar for the same module-storing-types shape:

```fun
sig x : I64; y : Bool end
```

For example:

```fun
(fn(m : sig x : I64 end) -> m.x)(module pub x = 1 end)
```

A module signature only matches public module members. A private binding does not satisfy a public requirement:

```fun
(fn(m : module pub x = I64 end) -> m.x)(module x = 1 end)
```

That program is rejected because `x` is private in the argument module.

Unannotated field access remains record-oriented for unknown receivers. Passing modules through functions requires an explicit module signature when the function accesses module members:

```fun
do
  S = module pub x = 42 end
  (fn(s : module pub x = I64 end) -> s.x)(S)
end
```
