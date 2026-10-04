# macro and syntax together
{
       macro twice(x) { quote($x + $x) };
       syntax wrap { wrap $x => twice($x) };
       wrap 10
     }
