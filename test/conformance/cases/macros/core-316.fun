# a Pattern parameter is read as a pattern
{
       macro matches(p : Pattern, e) { quote(match ($e) { $p => 1, _ => 0 }) };
       matches(Some(_), Some(3)) * 10 + matches(None, Some(3))
     }
