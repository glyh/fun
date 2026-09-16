# polymorphic record multiple instantiations
{ Pair = fn[A : Type, B : Type] { struct {fst: A; snd: B} }; p1 = Pair{fst = 10; snd = 20}; p2 = Pair{fst = True; snd = 3}; if (p2.fst) { p1.fst + p2.snd } else { 0 } }
