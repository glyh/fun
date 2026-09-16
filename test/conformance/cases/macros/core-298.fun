# an open supplying other names
{ M = module { pub x = 7 }; syntax answer { answer => 42 }; open M; x }
