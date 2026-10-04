# when False falls back
{
       syntax when {
       when $cond $branch else $fallback =>
           if ($cond) { $branch } else { $fallback }
       };
       when False 42 else 0
     }
