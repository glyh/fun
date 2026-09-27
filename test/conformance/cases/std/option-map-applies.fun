# Option's map leaves None alone and maps Some
match (Std.Options.map(fn(x) { x + 1 }, Some(41))) { Some(n) => n, None => 0 }
