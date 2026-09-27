# the supply form against a synonym an open put in scope: the head is a bare name,
# not a member path, and the supplied types still reach its parameters
{ M = module { pub pattern Two(a, b) = (a, b) };
  open M;
  f = fn(u, v) { match (v) { Two[I64, Bool](x, y) => 1 } };
  9 }
