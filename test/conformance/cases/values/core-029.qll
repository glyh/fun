# ref repeated closure increments
{ r = ref(0); inc = fn(_) { { n = deref(r); _ = r <- n + 1; deref(r) } }; _ = inc(); inc() }
