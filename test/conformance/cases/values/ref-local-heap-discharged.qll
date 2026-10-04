# a block whose reference stays inside it is pure, so it may stand in a type
{ f : Unit -> I64 = fn(u : Unit) { r = ref(1); _ = r <- 5; deref(r) }; f(()) }
