# a public member may not take a name an export took
{ M = module { pub x = 1 }; N = module { export M; pub x = 2 }; 0 }
