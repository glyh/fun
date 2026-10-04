# ref write read
{ r = ref(1); _ = r <- 2; deref(r) }
