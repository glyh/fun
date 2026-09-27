# the library's map applies in order
match (map(fn(x) { x + 1 }, Cons(1, Cons(2, Nil)))) { Cons(_, Cons(x, _)) => x, _ => 0 }
