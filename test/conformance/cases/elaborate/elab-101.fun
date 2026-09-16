{ rec R = fn[A : Type] { struct {x: (fn(A) { A })(I64)} }; R }
