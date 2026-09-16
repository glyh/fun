# a private member does not clash with an export
{ M = module { pub x = 1 }; N = module { x = 2; export M }; N.x }
