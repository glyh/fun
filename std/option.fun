# Options — what belongs here: everything a program does with an Option, reached as
# `Std.Options.f` or, after `open Std.Options`, as a bare `f`. The module is this
# unit's own public bindings; the compiler names nothing here.
#
# None is the absent case throughout: a default is written first (`get_or(d, o)`,
# `or_else(d, o)`) so it curries, and nothing panics.
Lib = import "std/lib";
open Lib;

# map(f, o) applies f to the value inside o; it is None for None.
pub map = fn[A : Type, B : Type](f : A -> B, o : Option(A)) : Option(B) {
  match (o) { Some(x) => Some(f(x)), None => None }
};

# bind(f, o) is f applied to the value inside o; it is None for None.
pub bind = fn[A : Type, B : Type](f : A -> Option(B), o : Option(A)) : Option(B) {
  match (o) { Some(x) => f(x), None => None }
};

# get_or(d, o) is the value inside o, or d for None.
pub get_or = fn[A : Type](d : A, o : Option(A)) : A {
  match (o) { Some(x) => x, None => d }
};

# or_else(d, o) is o itself when it holds a value; it is d for None.
pub or_else = fn[A : Type](d : Option(A), o : Option(A)) : Option(A) {
  match (o) { Some(_) => o, None => d }
};

# filter(p, o) keeps o's value when p answers True for it, and answers None
# otherwise; it is None for None.
pub filter = fn[A : Type](p : A -> Bool, o : Option(A)) : Option(A) {
  match (o) {
    Some(x) => match (p(x)) { True => Some(x), False => None },
    None => None
  }
};

# is_some(o) says whether o holds a value; it is False for None.
pub is_some = fn[A : Type](o : Option(A)) : Bool {
  match (o) { Some(_) => True, None => False }
};

# option_eq compares two options' contents with the element type's Eq; it is True for
# two Nones and False when one side is None and the other is Some.
pub impl option_eq : Eq(Option(a)) = module {
  fn eq(xs, ys) {
    match (xs) {
      Some(h) => match (ys) { Some(h2) => Eq.eq(h, h2), None => False },
      None => match (ys) { Some(_) => False, None => True }
    }
  }
};

