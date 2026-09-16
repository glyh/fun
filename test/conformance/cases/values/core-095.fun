# type-case struct field binder
{
     classify : Type -> I64 = fn(T) {
       match (T) { struct { x: I64; _ } => 1,
       struct { x: Bool; _ } => 2,
       struct { y: p; _ } => match (p) { String => 3, _ => 4 },
       _ => 0
       }
     };
     Point = struct {y: String; z: I64}; classify(Point)
   }
