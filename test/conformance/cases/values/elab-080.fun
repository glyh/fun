(fn(m : sig { x : I64 }) { open m; x + 1 })(module { pub x = 41 })
