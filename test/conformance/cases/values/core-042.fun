# rec not
{ rec f : Bool -> I64 = fn(x) { if (x) { 0 } else { f(not x) } }; f(False) }
