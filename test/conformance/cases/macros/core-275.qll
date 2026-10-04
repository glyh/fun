# nested syntax callsite inside parenthesized holes
{
       syntax is_zero { is_zero $x => $x == 0 };
       syntax inc { inc $x => $x + 1 };
       syntax wrap { wrap $x => $x + 1 };
       syntax choose {
       choose $cond then $branch else $fallback =>
           if ($cond) { $branch } else { $fallback }
       };
       choose (is_zero 0) then (wrap (inc 2)) else (wrap 10)
     }
