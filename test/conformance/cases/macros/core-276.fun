# nested syntax callsite inside parenthesised holes
{
       syntax bool {
       bool yes => True,
       bool no => False
       };
       syntax add2 { add2 $x => $x + 2 };
       syntax pick {
       pick $cond then $branch otherwise $fallback =>
           if ($cond) { $branch } else { $fallback }
       };
       pick (bool yes) then (add2 5) otherwise add2 10
     }
