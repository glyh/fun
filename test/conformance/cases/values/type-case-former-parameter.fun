# a type-case head that is a type former matches its parameters
({ Opt = fn(A : Type) { enum { Some(A), None } }; classify : Type -> I64 = fn(T) { match (T) { Opt(I64) => 1, Opt(x) => match (x) { Char => 2, _ => 3 }, _ => 0 } }; (classify(Opt(I64)), classify(Opt(Char)), classify(Opt(String)), classify(I64)) }).1
