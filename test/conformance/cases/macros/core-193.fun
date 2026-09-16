# macro multi-arg swap
{
       macro flip(a, b) { quote($b - $a) };
       flip(5, 3)
     }
