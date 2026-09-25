# the former's parameter must occur in its body (ruling 2026-09-25): A is applied
{ rec R = fn[A : Type] { struct {x: (fn(R){R})(A)} }; R }
