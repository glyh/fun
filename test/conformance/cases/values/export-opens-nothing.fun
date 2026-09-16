# an export opens nothing locally
{ x = 5; M = module { pub x = 1 }; N = module { export M; pub y = x }; N.y }
