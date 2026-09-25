# port-non-nominal-pattern-head: a type-former alias used as a pattern head keeps working -
# the head is tried by reduction before anything complains
{ Seq = fn(A : Type) { List(A) };
  rec sum = fn(l : Seq(I64)) { match (l) { Seq.Cons(x, xs) => x + sum(xs), Seq.Nil => 0 } };
  sum(Seq.Cons(1, Seq.Cons(2, Seq.Nil))) }
