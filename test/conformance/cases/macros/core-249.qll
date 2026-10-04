# a non-trailing hole is one term
{ syntax pick { pick $c then $t else $e => if ($c) { $t } else { $e } }; pick (1 < 2) then (40 + 2) else 0 }
