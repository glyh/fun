# type-case default_or String
{
     default_or : [T : Type] -> T -> T = fn[T : Type](fallback) {
       match (T) { I64 => 0,
       Bool => False,
       Unit => (),
       Char => 'a',
       String => "",
       _ => fallback
       }
     };
     default_or[String]("fallback") == ""
   }
