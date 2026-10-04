# type-case nominal classifier I64
{
     type Option a = Some a | None;
     classify : Type -> I64 = fn(T) {
       match (T) { Option(I64) => 1,
       Option _ => 2,
       _ => 0
       }
     };
     classify(Option(I64))
   }
