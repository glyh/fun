# nth past the end is None
match (Std.Lists.nth(2, Cons(1, Cons(2, Nil)))) { None => True, Some(_) => False }
