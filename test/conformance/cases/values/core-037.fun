# fix
{ rec f : Bool -> I64 = fn(x) { if (x) { 0 } else { f(True) } }; f(False) }
