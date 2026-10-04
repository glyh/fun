# a duplicated public member is rejected before any signature check runs
(fn(m : sig { x : I64 }) { m.x })(module { pub x = 'a'; pub x = 1 })
