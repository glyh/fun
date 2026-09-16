# module signature argument
(fn(m : sig { x : I64 }) { m.x })(module { pub x = 42 })
