# effect declared inside a quoted block expression keeps its body
{ macro m(_) { quote( { effect E = sig { op : Unit -> Unit }; 1 } ) }; m(0) }
