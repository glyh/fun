# part of a use's type parameters supplied, the rest determined by nothing: the scope
# end reports the one left over, as for an unsolved implicit
{ M = module { pub pattern Two(a, b) = (a, b) };
  f = fn(u, v) { match (v) { M.Two[I64](x, y) => 1 } };
  9 }
