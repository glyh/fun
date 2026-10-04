# quote splices an expression hole
{ macro m(e) { quote($e + 1) }; m(41) }
