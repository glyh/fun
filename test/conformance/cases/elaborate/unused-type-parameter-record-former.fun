# a record former with an unused parameter is refused like an enum former
# (ruling 2026-09-25)
{ Pair = fn[A, B] { struct { fst : A } }; 0 }
