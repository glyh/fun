{ bump : (r : Ref(I64)) ->{Mutate(r)} Unit = fn(r) { r <- deref(r) + 1 }; x = ref(1); _ = bump(x); deref(x) }
