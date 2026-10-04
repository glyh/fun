# reused expression hole duplicates evaluation
{
       r = ref(0);
       inc = fn(_) { { n = deref(r); _ = r <- n + 1; deref(r) } };
       syntax twice { twice $x => $x + $x };
       twice (inc())
     }
