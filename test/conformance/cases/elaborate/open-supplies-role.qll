# an open may not supply a member named like a syntactic role visible where it is written
{ M = module { pub answer = 7 }; syntax answer { answer => 42 }; open M; 1 }
