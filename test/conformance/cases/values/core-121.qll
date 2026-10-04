# a container's public members are unique: this is a duplicate member
{ M = module { pub x = 1; pub x = 2 }; M.x }
