# quote { } splices an expression hole into a declaration
{ macro define(v) : Decl { quote { pub answer = $v; } }; M = module { define(21 + 21) }; M.answer }
