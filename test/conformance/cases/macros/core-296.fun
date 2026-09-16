# a macro's own binder named like a syntax form
{ syntax answer { answer => 42 }; macro m(_) { quote({ answer = 7; 1 }) }; m(0) }
