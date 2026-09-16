{ effect Mutate = sig { op : I64 -> I64 }; sum_to : I64 -> I64 = fn(n) { acc = ref(0); acc <- n; deref(acc) }; r = ref(1); r <- 4; deref(r) + sum_to(1) }
