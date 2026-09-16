# an implicit function checked against an explicit type gets its implicit argument inserted
{ id = fn[A : Type](a : A) { a }; f : I64 -> I64 = id; f(3) }
