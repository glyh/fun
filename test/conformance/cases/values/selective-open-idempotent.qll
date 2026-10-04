# the same list twice changes nothing, and a wholesale open after it changes nothing
{ M = module { pub a = 1; pub b = 2 }; open M.{a}; open M.{a}; open M; a }
