# open M brings every public member, named or not
{ M = module { pub helper = fn(x) { x }; pub other = 7 }; open M; helper(other) }
