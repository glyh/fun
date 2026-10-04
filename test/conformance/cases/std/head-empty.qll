# head of the empty list is None, not a panic
match (Std.Lists.head(Nil[I64])) { None => True, Some(_) => False }
