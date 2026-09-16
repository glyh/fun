# an export may not take a name already public
{ M = module { pub x = 1 }; N = module { pub x = 2; export M }; 0 }
