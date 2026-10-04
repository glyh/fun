# a pin to a run-time value is tested by convertibility, on the ordered (Sequential) path
{ f = fn(w : Option(I64), x : I64) { match (w) { Some(^x) => 1, _ => 0 } };
  f(Some(5), 5) + (f(Some(9), 5) + f(Some(9), 9)) }
