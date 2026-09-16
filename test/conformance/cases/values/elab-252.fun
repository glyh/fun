{ bump : Ref(I64) ->{_} Unit = fn(r) { r <- deref(r) + 1 }; x = ref(1); _ = bump(x); _ = bump(x); deref(x) }
