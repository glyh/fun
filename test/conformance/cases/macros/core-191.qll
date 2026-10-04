# operator uses operands (Left assoc)
{
       order shift; infix (>>>) shift ($lhs, $rhs) { $lhs - $rhs };
       10 >>> 5 >>> 3
     }
