{ sum_to : I64 -> I64 = fn(n) { acc = ref(0); _ = acc <- deref(acc) + n; deref(acc) }; p : Tuple(sum_to(2), I64, Bool) = (1, True); p.1 }
