# a macro call with the wrong number of arguments is refused
{ macro m(x) { quote(x) }; m(1, 2) }
