# imported public parameterized effect handler
{ E = import "effects"; StateI64 = E.State(I64); match (perform StateI64.get(())) { x => x, effect StateI64.get () => 42 } }
