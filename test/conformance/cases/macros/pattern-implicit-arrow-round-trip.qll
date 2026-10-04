# a Pattern parameter carries the implicit-arrow form through reflection and back
{
  macro matches(p : Pattern, e) { quote(match ($e) { $p => 1, _ => 0 }) };
  matches([k] -> k -> k, [k : Type] -> k -> k) * 3
    + matches([k] -> Bool, [k : Type] -> Bool)
}
