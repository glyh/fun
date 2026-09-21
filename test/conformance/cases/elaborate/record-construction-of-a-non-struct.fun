# a record construction head must name a struct, not an unapplied type former
{ P = fn(A : Type) { struct { x : A } }; P{x = 1} }
