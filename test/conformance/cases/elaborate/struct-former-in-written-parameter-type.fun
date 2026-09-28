# a struct former applied to its argument is a valid written parameter type
{ Box = fn[A : Type] { struct { v : A; pub method get(r : Ref(I64)) : I64 { 3 } } };
  g = fn(o : Box[I64]) : I64 { o.v };
  b = Box[I64]{ v = 1 }; g(b) }
