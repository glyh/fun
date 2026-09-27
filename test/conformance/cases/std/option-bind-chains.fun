# Option's bind runs its function only on Some
match (option_bind(fn(x) { Some(x + 1) }, Some(41))) { Some(n) => n, None => 0 }
