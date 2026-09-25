# a type former as a pattern synonym's head is refused, as in the prototype (UnknownConstructor)
# - the former half of the sealed-head gap, which is parity rather than work
{ type Option2(a) = Some2(a) | None2;
  M = module { pub pattern IsOpt(a) = Option2(a) };
  1 }
