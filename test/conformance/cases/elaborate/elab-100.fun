{ rec R = fn[A : Type] { struct {x: (fn(R) { R })(I64)} }; R }
