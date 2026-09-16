# struct private helper
{ M = module { secret = 10; pub x = secret + 1 }; M.x }
