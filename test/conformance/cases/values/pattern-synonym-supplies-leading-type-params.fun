# a use supplies only its leading type parameters; the scrutinee's type solves the rest
{ M = module { pub pattern Two(a, b) = (a, b) };
  match ((1, True)) { M.Two[I64](x, y) => y } }
