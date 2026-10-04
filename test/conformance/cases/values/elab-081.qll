(fn(m : sig { x : I64 }) { open m; x + 1 })(module { pub y = 5; pub x = 41 })
