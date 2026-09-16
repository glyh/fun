# type-case type_name String
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
     type_name(String) == "string"
   }
