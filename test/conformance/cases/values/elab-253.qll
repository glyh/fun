{ sum_to : I64 -> I64 = fn(n) { acc = ref(0); _ = acc <- deref(acc) + n; deref(acc) }; sum_to(3) }
