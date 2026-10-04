{ rec Odd = fn[A : Type, B : Type] { struct {x: Option(Odd[B, A])} }; Odd }
