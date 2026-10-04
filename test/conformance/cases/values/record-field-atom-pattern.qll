# an atom record field pattern whose type is the field's type matches at the call (x = 2)
{ R = struct { f : I64; g : I64 };
  f = fn(x : I64, y : match (R{f = 1; g = x}) { R{f = 1; g = 2} => I64, _ => Char }) { y };
  f(2, 5) }
