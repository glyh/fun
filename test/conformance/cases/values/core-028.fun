# ref closure observes later write
{ r = ref(0); f = fn(_) { deref(r) }; _ = r <- 4; f() }
