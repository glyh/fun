# binder hole can introduce use-site name
{
       syntax bind {
       bind $(name : Id) $value in $body => { $name = $value; $body }
       };
       bind x 3 in x
     }
