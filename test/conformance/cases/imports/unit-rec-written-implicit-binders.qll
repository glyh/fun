open (import "std");
open (import "lists");
same : [A : Eq] -> A -> A -> Bool = fn[A : Type](x, y) { Eq.eq(x, y) };
# through the trait: the unit's impl drives the binder-on-the-type helper
if (same[List(I64)](Cons(1, Cons(2, Nil)), Cons(1, Cons(2, Nil)))) {
  # the binder-on-the-lambda spelling, called directly
  if (lenrec[I64](Cons(7, Cons(8, Cons(9, Nil)))) == 3) {
    # the program-level control: the same helper shape here, unrefused as ever
    rec progrec : [B : Eq] -> List(B) -> List(B) -> Bool = fn[B : Type](xs, ys) {
      match (xs) {
        Nil => match (ys) { Nil => True, Cons(_, _) => False },
        Cons(h, t) => match (ys) {
          Nil => False,
          Cons(h2, t2) => if ((==)[B](h, h2)) { progrec[B](t, t2) } else { False }
        }
      }
    };
    if (progrec[I64](Cons(1, Nil), Cons(1, Nil))) { 42 } else { 43 }
  } else { 44 }
} else { 45 }
