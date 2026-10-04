# a pattern synonym's generalized types are its implicit type parameters: a use may
# supply them, the way it supplies a call's implicits. The scrutinee's type here is a
# bare meta (an unannotated lambda parameter), so nothing else determines them
{ M = module { pub pattern Two(a, b) = (a, b) };
  f = fn(u, v) { match (v) { M.Two[I64, Bool](x, y) => 1 } };
  9 }
