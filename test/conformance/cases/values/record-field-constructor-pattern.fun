# a constructor record field pattern whose payload matches (atom-field-pattern control)
{ R = struct { f : I64; g : Option(I64) };
  f = fn(x : Option(I64), y : match (R{f = 1; g = x}) { R{f = 1; g = Some(z)} => I64, _ => Char }) { y };
  f(Some(5), 5) }
