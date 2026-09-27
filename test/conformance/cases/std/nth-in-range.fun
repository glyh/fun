# nth counts from 0
match (Std.Lists.nth(1, Cons(1, Cons(2, Nil)))) { Some(x) => x, None => 0 }
