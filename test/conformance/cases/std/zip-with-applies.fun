# zip_with truncates like zip and applies its function pairwise
match (Std.Lists.zip_with(fn(x, y) { x + y }, Cons(1, Cons(2, Nil)), Cons(10, Nil))) { Cons(x, _) => x, _ => 0 }
