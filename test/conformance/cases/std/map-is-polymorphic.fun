# map's element type is not fixed to I64
match (map(fn(b) { not(b) }, Cons(True, Cons(False, Nil)))) { Cons(_, Cons(x, _)) => x, _ => True }
