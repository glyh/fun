# open M.{helper} brings the named value, and only it
{ M = module { pub helper = fn(x) { x }; pub other = 1 }; open M.{helper}; helper(3) }
