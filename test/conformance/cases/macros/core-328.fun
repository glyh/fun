# a macro's result applied to a further argument
{ macro ident(_) { quote(fn(y) { y }) }; ident(0)(5) }
