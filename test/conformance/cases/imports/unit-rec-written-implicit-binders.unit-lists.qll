# A unit's top level holds a recursive helper whose implicit binder is written out,
# in both spellings: on the declaration's type (eqrec) and on the lambda itself with
# typed parameters and a written result type (lenrec). Both were refused here with
# "cannot unify VPi with VPi" while working at a program's top level: a module
# binding bound the helper's own name at a meta instead of its written type, so the
# body's self-calls could not insert the hidden dictionary the trait bound hides.
open (import "std");
pub rec eqrec : [B : Eq] -> List(B) -> List(B) -> Bool = fn[B : Type](xs, ys) {
  match (xs) {
    Nil => match (ys) { Nil => True, Cons(_, _) => False },
    Cons(h, t) => match (ys) {
      Nil => False,
      Cons(h2, t2) => if ((==)[B](h, h2)) { eqrec[B](t, t2) } else { False }
    }
  }
};
pub rec lenrec = fn[A : Type](xs : List(A)) : I64 {
  match (xs) { Nil => 0, Cons(_, t) => 1 + lenrec[A](t) }
};
# the direct helper serves an impl, so a duplicate open quotes it too
pub impl eq_list_direct : Eq(List(I64)) = module { fn eq(xs, ys) { eqrec[I64](xs, ys) } };
