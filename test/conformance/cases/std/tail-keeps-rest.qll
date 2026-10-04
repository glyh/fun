# tail is the list without its first element
match (Std.Lists.tail(Cons(1, Cons(2, Nil)))) { Some(t) => Std.Lists.length(t), None => 0 }
