# type-case default_or nominal fallback
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
     type Color = Red | Blue; match (default_or[Color](Blue)) { Red => 1, Blue => 2 }
   }
