# the former's parameter must occur in its body (ruling 2026-09-25): the outer A
# is applied, under the shadowing binder of the same name
{ rec R = fn[A : Type] { struct {x: (fn(A){A})(A)} }; R }
