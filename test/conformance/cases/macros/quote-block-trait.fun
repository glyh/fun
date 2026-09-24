# trait declared inside a quoted block expression keeps its body
{ macro m(_) { quote( { trait T(A) = sig { f : A -> I64 }; 1 } ) }; m(0) }
