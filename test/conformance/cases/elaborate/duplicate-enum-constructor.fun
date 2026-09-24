# a constructor name may not repeat within one enum
{ M = module { pub rec E = enum { A(I64), A } }; 0 }
