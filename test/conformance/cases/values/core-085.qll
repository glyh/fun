# type-case type_name I64
{
     type_name : Type -> String = fn(T) {
       match (T) { I64 => "i64",
       Bool => "bool",
       Char => "char",
       Unit => "unit",
       String => "string",
       _ => "other"
       }
     };
     type_name(I64) == "i64"
   }
