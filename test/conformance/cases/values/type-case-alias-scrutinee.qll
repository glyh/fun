# a type-case on an alias of the matched variable refines it
({ f : [T : Type] -> T -> I64 = fn[T](x) {
     U = T;
     match (U) { I64 => x + 1, _ => 0 } };
   f(7) })
