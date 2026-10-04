# type-case default I64
{
     default : [T : Type] -> T = fn[T : Type] {
       match (T) { I64 => 0,
       Bool => False,
       Unit => (),
       Char => 'a',
       _ => panic("no default")
       }
     };
     default[I64]
   }
