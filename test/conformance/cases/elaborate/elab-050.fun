{ choose : Type -> Type -> Bool -> Type = fn(a, b, c) { if (c) { a } else { b } }; (42 : choose(I64, Bool, True)) }
