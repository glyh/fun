# duplicate module field resolves to last
{ M = module { pub x = 1; pub x = 2 }; M.x }
