# parameterized method uses self
{ Pair = fn[A : Type, B : Type] { struct { fst: A; snd: B; pub method swap() { (self.snd, self.fst) } } }; (Pair[I64, Bool].swap(Pair[I64, Bool]{fst = 1; snd = True})).0 }
