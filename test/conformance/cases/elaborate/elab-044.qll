{ classify : Type -> I64 = fn(T) { match (T) { struct { x: p; _ } => match (p) { I64 => 1, _ => 2 }, _ => 0 } }; classify }
