# a type-case on T narrows a value of type T in the branch
({ f : [T : Type] -> T -> I64 = fn[T : Type](x) { match (T) { I64 => x, _ => 0 } }; (f(7), f('a')) }).0
