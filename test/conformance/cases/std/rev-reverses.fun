# the library's rev: the last element comes first
match (rev(Cons(1, Cons(2, Cons(3, Nil))))) { Cons(h, _) => h, _ => 0 }
