# one generalized synonym instantiated at two different scrutinee types
{ M = module { pub pattern Two(a, b) = (a, b) }; match (1, True) { M.Two(a, b) => a } + match (True, 2) { M.Two(a, b) => b } }
