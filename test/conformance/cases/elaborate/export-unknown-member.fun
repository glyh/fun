# a selected name must be a public member
{ M = module { pub x = 1 }; N = module { export M.{y} }; 0 }
