# a pattern synonym whose struct type-case inspects a nested field's type
{ M = module { pub pattern HasX(a) = struct { x: struct { y: a; _ }; _ } }; match ((struct { x: struct { y: I64; z: Bool }; w: Bool }) : Type) { M.HasX(t) => 1, _ => 0 } }
