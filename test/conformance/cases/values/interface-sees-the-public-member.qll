# a private binding may shadow a public one; the interface still holds the public member
(fn(m : sig { x : I64 }) { m.x })(module { pub x = 1; x = 'a' })
