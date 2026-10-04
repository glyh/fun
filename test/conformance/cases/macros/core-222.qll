# operator right assoc
{
       order rshift : assoc(right); infix (<<<) rshift ($lhs, $rhs) { $lhs - $rhs };
       10 <<< 5 <<< 3
     }
