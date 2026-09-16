# parameterized record type declaration
{ Pair = fn[A : Type, B : Type] { struct {fst: A; snd: B} }; (Pair{fst = 1; snd = True}).snd }
