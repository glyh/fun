# imported parameterized handler distinguishes instances
{ E = import "effects"; StateI64 = E.State(I64); StateBool = E.State(Bool); match (if (perform StateBool.get(())) { perform StateI64.get(()) } else { 0 }) { x => x, effect StateI64.get () => resume(1), effect StateBool.get () => resume(True) } }
