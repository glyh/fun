# a module in type position: a refusal, not a signature
(fn(m : module { pub x = I64 }) { m.x })(module { pub x = 42 })
