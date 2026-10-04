# type-case nominal full application
{ type Option a = Some a | None;
     match (Option(I64)) { Option(I64) => 1,
     Option(Bool) => 2,
     _ => 3
     }
     }
