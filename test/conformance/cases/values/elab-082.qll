(fn(m : sig { x : I64 }) { fn(u : I64) { open m; x + u } })(module { pub x = 41 })(1)
