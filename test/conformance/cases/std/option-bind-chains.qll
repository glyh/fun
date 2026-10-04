# Option's bind runs its function only on Some
match (Std.Options.bind(fn(x) { Some(x + 1) }, Some(41))) { Some(n) => n, None => 0 }
