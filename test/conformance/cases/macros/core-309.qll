# export M
{ M = module { pub x = 1; pub y = 2 }; N = module { export M }; N.x + N.y }
