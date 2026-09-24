# a private rebinding is not a duplicate: outside sees the public x, the body sees the private one
{ M = module { pub x = 1; x = 2; pub y = x }; M.y * 10 + M.x }
