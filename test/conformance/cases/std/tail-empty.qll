# tail of the empty list is None
match (Std.Lists.tail(Nil[I64])) { None => True, Some(_) => False }
