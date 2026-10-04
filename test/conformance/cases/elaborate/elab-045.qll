{ classify : Type -> I64 = fn(T) { match (T) { struct { x: I64; _ } => 1, struct { x: Bool; _ } => 2, _ => 0 } }; classify }
