# generated syntax forms are usable in generated body
{
       syntax build_choose {
       build_choose => {
           syntax flag {
           flag yes => True,
           flag no => False
           };
           syntax choose {
           choose $cond then $branch else $fallback =>
               if ($cond) { $branch } else { $fallback }
           };
           choose (flag yes) then (40 + 2) else 0
         }
       };
       build_choose
     }
