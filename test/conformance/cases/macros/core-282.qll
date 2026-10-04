# pattern hole splices a use-site pattern and its binders
{
       syntax unwrap_or {
       unwrap_or $v $(p : Pattern) $body $d => match ($v) { $p => $body, _ => $d }
       };
       unwrap_or (Some(4)) (Some(x)) x 0
     }
