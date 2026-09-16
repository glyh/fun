{ rec f = fn(xs : List(I64)) : I64 { match (xs) { Cons(m, Nil) => m, Cons(m, rest) => f(rest), Nil => 0 } }; f(Cons(1, Cons(2, Nil))) }
