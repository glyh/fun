# a later arm an earlier arm subsumes structurally is unreachable (rule 8, hard error)
{ classify = fn(v : Option(I64)) { match (v) { Some(a) => 1, Some(1) => 2, _ => 0 } }; classify(Some(1)) }
