# Eq over lists is structurally recursive over two values: an arity-2 recursive
# helper under a pub impl. Quoting the impl's evidence (the duplicate check a
# second open of the same unit reaches) must read the helper's deferred call
# back as the call, not unfold it per argument -- this used to core the runner.
open (import "std");
pub rec probe_go = fn(xs : List(I64), ys : List(I64)) : Bool {
  match (xs) {
    Nil => match (ys) { Nil => True, Cons(_, _) => False },
    Cons(h, t) => match (ys) {
      Nil => False,
      Cons(h2, t2) => match (i64_to_bool(eq_i64(h, h2))) { True => probe_go(t, t2), False => False }
    }
  }
};
pub impl probe : Eq(List(I64)) = module { fn eq(xs, ys) { probe_go(xs, ys) } };
