# a pattern synonym over a struct type-case pattern, used in a match: the direct-match path runs
{ M = module { pub pattern HasX(a) = struct { x: a; _ } }; match ((struct { x: I64; y: Bool }) : Type) { M.HasX(t) => 1, _ => 0 } }
