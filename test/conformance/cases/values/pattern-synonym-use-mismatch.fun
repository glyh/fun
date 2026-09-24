# a use whose scrutinee type does not fit the synonym: the use errors, not the declaration
{ M = module { pub pattern Two(a, b) = (a, b) }; match (1) { M.Two(x, y) => x } }
