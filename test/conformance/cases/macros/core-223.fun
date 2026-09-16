# operator mixed precedence
{
       infix (+++) additive ($lhs, $rhs) { $lhs + $rhs };
       infix (***) multiplicative ($lhs, $rhs) { $lhs * $rhs };
       2 +++ 3 *** 4
     }
