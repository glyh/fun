# export M.{x}
{ M = module { pub x = 1; pub y = 2 }; N = module { export M.{x} }; N.x }
