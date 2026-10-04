# a group is left-associative by default
{ order g; infix (@@) g ($a, $b) { $b }; infix (%%) g ($a, $b) { ($a, $b) }; (1 %% 2 @@ 3) }
