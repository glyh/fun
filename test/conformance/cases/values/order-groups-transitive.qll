# precedence is transitive: high is stronger than low through mid
{ order low; order mid : stronger_than(low); order high : stronger_than(mid); infix (@@) low ($a, $b) { ($a, $b) }; infix (%%) high ($a, $b) { $b }; (1 @@ 2 %% 3).1 }
