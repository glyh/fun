# parameterized record method
{ Pair = fn[A : Type, B : Type] { struct { fst: A; snd: B; pub swap = fn(p) { (p.snd, p.fst) } } }; (Pair[I64, Bool].swap(Pair[I64, Bool]{fst = 1; snd = True})).0 }
