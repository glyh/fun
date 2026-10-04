# head of a non-empty list is its first element
match (Std.Lists.head(Cons(7, Nil))) { Some(x) => x, None => 0 }
