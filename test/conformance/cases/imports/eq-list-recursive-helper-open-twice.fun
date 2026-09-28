# two opens of the same unit: the second reaches the duplicate-impl check that
# quotes the evidence, and the comparison itself runs the recursive helper
open (import "mylists"); open (import "mylists");
same : [A : Eq] -> A -> A -> Bool = fn[A : Type](x, y) { Eq.eq(x, y) };
if (same[List(I64)](Cons(1, Cons(2, Nil)), Cons(1, Cons(2, Nil)))) { 108 } else { 109 }
