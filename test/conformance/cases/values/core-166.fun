# inline module open
{ B = module { pub x = 3 }; M = module { open B; pub y = x }; M.y }
