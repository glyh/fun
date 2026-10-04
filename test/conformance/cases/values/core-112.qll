# recursive list sum
{ type MyList(a) = Cons(a, MyList(a)) | Nil; rec sum : MyList(I64) -> I64 = fn(xs) { match (xs) { Cons(x, rest) => x + sum(rest), Nil => 0 } }; sum(Cons(1, Cons(2, Cons(3, Nil)))) }
