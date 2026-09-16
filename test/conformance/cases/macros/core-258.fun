# a group unrelated to the prelude's meets <-
{ order mine; infix (<>) mine ($a, $b) { $a; $b }; r = ref(0); _ = r <- 1 <> 5; deref(r) }
