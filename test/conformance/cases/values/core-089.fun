# type-case nominal complex param pattern
{ type Option a = Some a | None;
     match (Option(Option(I64))) { Option(Option(I64) | I64) => 1,
     Option _ => 2,
     _ => 3
     }
     }
