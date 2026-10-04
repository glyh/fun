# two matching heads neither of which is an instance of the other are ambiguous
{ trait Conv(a) = sig { conv : a -> I64 }; impl Conv(I64 -> a) = module { conv = fn(p) { 1 } }; impl Conv(b -> Bool) = module { conv = fn(p) { 2 } }; Conv.conv(fn(x : I64) { True }) }
