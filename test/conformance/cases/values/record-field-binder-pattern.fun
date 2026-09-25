# a bare-binder record field pattern matches any field value (atom-field-pattern control)
{ R = struct { f : I64; g : I64 };
  f = fn(x : I64, y : match (R{f = 1; g = x}) { R{f = 1; g = y} => I64, _ => Char }) { y };
  f(5, 5) }
