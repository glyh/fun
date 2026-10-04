# a stated relation to a weakest group overrides
{ order mine : weaker_than(assignment); infix (<>) mine ($a, $b) { $a; $b }; r = ref(0); _ = r <- 1 <> 5; deref(r) }
