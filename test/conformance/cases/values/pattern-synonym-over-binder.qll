# a pattern synonym that is a bare binder: the scrutinee's type is generalized
{ M = module { pub pattern Id(x) = x }; match (1) { M.Id(y) => y } }
