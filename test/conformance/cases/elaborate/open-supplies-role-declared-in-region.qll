# an open may not supply a member named like a syntactic role declared in its region
{ M = module { pub answer = 7 }; open M; syntax answer { answer => 42 }; 1 }
