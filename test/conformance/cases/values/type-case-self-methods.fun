# type-case refinement reaches the enclosing struct's self methods
({ f : [T : Type] -> T -> I64 = fn[T](x) {
     C = struct { v : T; pub method get() : T { self.v }; pub method g() : I64 { match (T) { I64 => self.get() + 1, _ => 0 } } };
     C{v = x}.g() };
   f(7) })
