# checking against an implicit function type binds its implicit parameter first
{ id = fn[A : Type](a : A) { a }; g : [A : Type] -> A -> A = id; g(5) }
