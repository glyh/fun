# impl declared inside a quoted block expression keeps its body
{ trait Size(a) = sig { size : a -> I64 }; macro m(_) { quote( { impl Size(I64) = module { size = fn(n) { 3 } }; 1 } ) }; m(0) }
