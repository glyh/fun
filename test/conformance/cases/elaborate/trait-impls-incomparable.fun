# two matching heads neither of which is an instance of the other are ambiguous
{ trait Conv(A) = sig { conv : A -> I64 }; impl Conv(I64 -> A) = module { conv = fn(p) { 1 } }; impl Conv(B -> Bool) = module { conv = fn(p) { 2 } }; Conv.conv(fn(x : I64) { True }) }
