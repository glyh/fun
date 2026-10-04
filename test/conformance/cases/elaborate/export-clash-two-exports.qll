# two exports may not take the same name
{ A = module { pub x = 1 }; B = module { pub x = 2 }; N = module { export A; export B }; 0 }
