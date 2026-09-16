# quote { } splices a declaration hole
{ macro wrap(v) : List(Decl) { d = quote { pub answer = $v }; quote { $d; pub other = 1; } };
       M = module { wrap(5) }; M.answer + M.other }
