# a struct-type pattern binds a field's type, matched again inside the branch
({ classify : Type -> I64 = fn(T) { match (T) { struct { x: I64; _ } => 1, struct { y: p; _ } => match (p) { String => 3, _ => 4 }, _ => 0 } }; P = struct { y: String; z: I64 }; Q = struct { x: I64 }; (classify(P), classify(Q), classify(I64)) }).0
