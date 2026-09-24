# a pattern synonym over a constructor whose parameter type is not fixed: generalized, not refused
{ M = module { pub pattern Head(a) = Option.Some(a) }; 1 }
