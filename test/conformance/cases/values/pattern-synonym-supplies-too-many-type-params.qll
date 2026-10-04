# more type parameters supplied than the synonym takes
{ M = module { pub pattern Two(a, b) = (a, b) };
  match ((1, True)) { M.Two[I64, Bool, Char](x, y) => x } }
