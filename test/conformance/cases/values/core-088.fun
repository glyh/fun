# type-case nominal param bind
{ type Option a = Some a | None;
     match (Option(I64)) { Option x => match (x) { I64 => 1, _ => 2 },
     _ => 3
     }
     }
