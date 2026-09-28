# trait declared inside a quoted block expression keeps its body
{ macro m(_) { quote( { trait T(a) = sig { f : a -> I64 }; 1 } ) }; m(0) }
