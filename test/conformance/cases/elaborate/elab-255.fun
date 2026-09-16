{ outer = ref(0); f = fn(u : Unit) { b = ref(2); xs = Cons(outer, Cons(b, Nil)); 1 }; (f : Unit -> I64) }
