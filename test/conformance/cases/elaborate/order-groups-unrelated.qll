# two operators in groups with no declared order do not combine without parentheses
{ order a; order b; infix (@@) a ($x, $y) { $x }; infix (%%) b ($x, $y) { $y }; 1 @@ 2 %% 3 }
