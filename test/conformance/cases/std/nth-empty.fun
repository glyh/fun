# nth of the empty list is None
match (Std.Lists.nth(0, Nil[I64])) { None => True, Some(_) => False }
