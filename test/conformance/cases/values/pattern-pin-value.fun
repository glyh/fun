# a pin names an existing value: the payload is tested against the outer `x`, not bound
{ v = Some(5); x = 5; match (v) { Some(^x) => 1, _ => 0 } }
