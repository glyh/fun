# nested syntax callsite in repeated hole
{
       syntax inc { inc $x => $x + 1 };
       syntax triple { triple $x => $x + $x + $x };
       triple (inc (inc 2))
     }
