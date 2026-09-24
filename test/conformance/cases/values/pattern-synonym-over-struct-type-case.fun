# a pattern synonym over a struct type-case pattern: the right-hand side is carried into the matcher
{ M = module { pub pattern HasX(a) = struct { x: a; _ } }; 1 }
