# a selection exports only the names it lists
{ M = module { pub x = 1; pub y = 2 }; N = module { export M.{x} }; N.y }
