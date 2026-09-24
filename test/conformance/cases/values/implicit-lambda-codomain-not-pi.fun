# an implicit lambda whose expected codomain is not a function type binds its parameter itself
{ f : [A : Type] -> I64 = fn[T : Type] { match (T) { I64 => 1, _ => 0 } }; f[Bool] }
