# type-case Char a
{
     is_zeroish : [T : Type] -> T -> Bool = fn[T : Type](x) {
       match (T) { I64 => x == 0,
       Bool => x == False,
       Unit => True,
       Char => x == 'a',
       _ => False
       }
     };
     is_zeroish('a')
   }
