# port-non-nominal-pattern-head: a pattern head that is a member of a non-nominal is an error,
# because its reduction yields no nominal
{ M = module { pub f = fn(x : I64) { x } };
  g = fn(u : I64) { match (u) { M.f(a) => a, _ => 0 } };
  g(3) }
