{ r = ref(0); f = fn(u : Unit) { b = ref(1); xs = Cons(r, Cons(b, Nil)); r <- 5 }; (f : Unit -> Unit) }
