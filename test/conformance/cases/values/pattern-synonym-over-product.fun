# a pattern synonym over a product: the scrutinee's tuple type is generalized, not refused
{ M = module { pub pattern Two(a, b) = (a, b) }; match (1, True) { M.Two(x, y) => x } }
