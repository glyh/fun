# a lowercase alias is pinned, not renamed: `^z` names `z = I64`, which the case rule would otherwise bind
{ z = I64;
  classify = fn(T : Type) { match (T) { Option(^z) => 1, _ => 0 } };
  classify(Option(I64)) + classify(Option(Bool)) }
