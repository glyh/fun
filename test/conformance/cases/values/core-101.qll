# handler value branch bubbles outer effect
{
      effect Inner = sig { value : Unit -> I64 };
      effect Outer = sig { value : Unit -> I64 };
     match (match (0) { x => perform Outer.value(()),
       effect Inner.value () => 0
       }) { x => x,
     effect Outer.value () => 42
     }
     }
