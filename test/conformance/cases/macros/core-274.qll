# generated syntax form can bind its own holes and branches
{
       syntax make_bounded {
       make_bounded $limit => {
           syntax bound {
           bound $x below => if ($x < $limit) { $x } else { $limit },
           bound $x above => if ($x > $limit) { $x } else { $limit }
           };
           bound 2 below + bound 9 above
         }
       };
       make_bounded 5
     }
