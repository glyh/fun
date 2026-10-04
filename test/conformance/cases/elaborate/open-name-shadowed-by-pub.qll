# an open's names are not members of this container, so a public member may shadow one
{ N = module { pub x = 1 }; M = module { open N; pub x = 2 }; M.x }
