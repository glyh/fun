# type-case struct closed rejects extra
{ Point = struct {x: I64; y: Bool};
     match (Point) { struct { x: I64 } => 1,
     struct { x: I64; _ } => 2,
     _ => 3
     }
     }
