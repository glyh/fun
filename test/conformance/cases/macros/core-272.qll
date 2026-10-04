# generated syntax form does not shadow caller syntax
{
       syntax tag { tag $x => $x + 1 };
       syntax make_tag {
       make_tag => {
           syntax tag { tag $x => $x + 2 };
           tag 2
         }
       };
       make_tag + tag 1
     }
