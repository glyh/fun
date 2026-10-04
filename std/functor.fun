# Functors — what belongs here: the `Functor` trait and its impls for the prelude's type
# constructors, reached as `Std.Functors.Functor` or, after `open Std.Functors`, as a bare
# `Functor`. The module is this unit's own public bindings; the compiler names nothing here.
#
# The trait's one parameter is a type *constructor*, not a type: `f(A)` in `map`'s
# signature is what makes `Functor` a trait over `List` and `Option` themselves, so one
# `map` serves every container instead of one per type. The impls are exported by
# `std/stage2` alongside the equality impls, so a use infers the constructor from its
# argument and needs no written type.
#
# Each public binding carries .NET XML doc comments (on `##` lines — a line comment,
# distinct from the ordinary `#` comment): <summary>, and an <example> whose <code> line
# ends in "// returns V", the value the expression must evaluate to.
# test/run-doc-examples.sh runs every such line through the conformance runner and
# compares against V, so the examples are tested behaviour. The runner prints a
# constructor's name alone, so a list or an option is compared with `==` rather than
# returned.
Lib = import "std/lib";
open Lib;
Lists = import "std/list";
Options = import "std/option";

## <summary>The type constructors whose elements can be mapped over.</summary>
## <remarks>map(g, xs) applies g inside the container, leaving its shape alone.</remarks>
pub trait Functor(f) = sig {
  map : [A : Type, B : Type] -> (A -> B) -> f(A) -> f(B)
};

## <summary>List is a functor.</summary>
## <example>
## <code>Std.Functors.Functor.map(fn(x) { x + 1 }, Cons(1, Cons(2, Nil))) == Cons(2, Cons(3, Nil)) // returns True</code>
## </example>
pub impl list_functor : Functor(List) = module {
  fn map[A : Type, B : Type](g : A -> B, xs : List(A)) : List(B) { Lists.map(g, xs) }
};

## <summary>Option is a functor.</summary>
## <example>
## <code>Std.Functors.Functor.map(fn(x) { x + 1 }, Some(1)) == Some(2) // returns True</code>
## </example>
pub impl option_functor : Functor(Option) = module {
  fn map[A : Type, B : Type](g : A -> B, o : Option(A)) : Option(B) { Options.map(g, o) }
};
