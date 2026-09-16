# <- is weaker than arithmetic
{ r = ref(0); _ = r <- 1 + 2; deref(r) }
