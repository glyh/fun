# a literal inside a bracketed capture stays in it
{ syntax when { when $c $t else $e => if ($c) { $t } else { $e } };
       when True (if (False) { 1 } else { 2 }) else 0 }
