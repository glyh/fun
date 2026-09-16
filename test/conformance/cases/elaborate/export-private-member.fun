# an export takes only public members
{ M = module { x = 1; pub y = 2 }; N = module { export M }; N.x }
