# the library's fold accumulates left to right
fold(fn(acc, x) { acc + x }, 0, Cons(1, Cons(2, Cons(3, Nil))))
