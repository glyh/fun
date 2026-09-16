# a group declared assoc(none) does not chain
{ order once : assoc(none); infix (@@) once ($a, $b) { $a }; 1 @@ 2 @@ 3 }
