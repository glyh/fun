# reverse puts the last element first
match (Std.Lists.reverse(Cons(1, Cons(2, Cons(3, Nil))))) { Cons(h, _) => h, _ => 0 }
