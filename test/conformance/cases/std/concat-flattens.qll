# concat splices a list of lists together in order
match (Std.Lists.concat(Cons(Cons(1, Nil), Cons(Cons(2, Nil), Nil)))) { Cons(_, Cons(x, _)) => x, _ => 0 }
