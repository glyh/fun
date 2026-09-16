# handler parameterized dispatch
{
      effect State(S) = sig { get : Unit -> S };
     StateI64 = State(I64);
     StateBool = State(Bool);
     match (if (perform StateBool.get(())) { perform StateI64.get(()) + 1 } else { 0 }) { x => x,
     effect StateI64.get () => resume(10),
     effect StateBool.get () => resume(True)
     }
     }
