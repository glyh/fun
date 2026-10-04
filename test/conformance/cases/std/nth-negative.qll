# a negative index finds nothing
match (Std.Lists.nth(0 - 1, Cons(1, Nil))) { None => True, Some(_) => False }
