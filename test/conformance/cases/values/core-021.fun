# module signature extra field
(fn(m : sig { x : I64 }) { m.x })(module { pub x = 42; pub y = True })
