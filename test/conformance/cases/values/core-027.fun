# ref aliases share cell
{ r = ref(1); alias = r; _ = alias <- 3; deref(r) }
