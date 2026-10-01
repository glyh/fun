# a type-case head giving more type parameters than the type takes
{ Opt = fn(A : Type) { enum { Some(A), None } }; classify = fn(T : Type) { match (T) { Opt(I64, Bool) => 1, _ => 0 } }; classify(Opt(I64)) }
