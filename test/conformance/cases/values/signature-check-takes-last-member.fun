# checking a module against a signature takes the last member of a name (I3)
(fn(m : sig { x : I64 }) { m.x })(module { pub x = 'a'; pub x = 1 })
