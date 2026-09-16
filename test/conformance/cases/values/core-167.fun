# struct-level open
{ M = module { pub k = 7 }; S = struct { open M; pub m = k }; S.m }
