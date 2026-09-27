# The library's list and option functions: the first cut of the public surface.
# The compiler does not name anything here; only the ABI in std/bootstrap does.
Core = import "std/bootstrap";
open Core;

pub rev = fn[A : Type](l : List(A)) : List(A) {
  rec go = fn(l : List(A), acc : List(A)) : List(A) {
    match (l) { Nil => acc, Cons(h, t) => go(t, Cons(h, acc)) }
  };
  go(l, Nil)
};

pub append = fn[A : Type](a : List(A), b : List(A)) : List(A) {
  rec go = fn(a : List(A)) : List(A) {
    match (a) { Nil => b, Cons(h, t) => Cons(h, go(t)) }
  };
  go(a)
};

pub map = fn[A : Type, B : Type](f : A -> B, l : List(A)) : List(B) {
  rec go = fn(l : List(A)) : List(B) {
    match (l) { Nil => Nil, Cons(h, t) => Cons(f(h), go(t)) }
  };
  go(l)
};

pub fold = fn[A : Type, B : Type](f : B -> A -> B, z : B, l : List(A)) : B {
  rec go = fn(l : List(A), acc : B) : B {
    match (l) { Nil => acc, Cons(h, t) => go(t, f(acc, h)) }
  };
  go(l, z)
};

pub option_map = fn[A : Type, B : Type](f : A -> B, o : Option(A)) : Option(B) {
  match (o) { Some(x) => Some(f(x)), None => None }
};

pub option_bind = fn[A : Type, B : Type](f : A -> Option(B), o : Option(A)) : Option(B) {
  match (o) { Some(x) => f(x), None => None }
};
