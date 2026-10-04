# the library's append: b follows a
match (Std.Lists.append(Cons(1, Nil), Cons(2, Cons(3, Nil)))) { Cons(_, Cons(x, _)) => x, _ => 0 }
