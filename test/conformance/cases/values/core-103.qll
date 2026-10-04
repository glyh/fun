# handler outer handles residual effect
{
      effect Inner = sig { hit : I64 -> I64 };
      effect Outer = sig { hit : I64 -> I64 };
     match (match (perform Outer.hit(1)) { x => x,
       effect Inner.hit n => resume(n)
       }) { x => x,
     effect Outer.hit n => n + 41
     }
     }
