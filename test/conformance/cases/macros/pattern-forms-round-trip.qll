# a Pattern parameter carries the pin, arrow, universe and tuple forms through reflection and back
{
  macro matches(p : Pattern, e) { quote(match ($e) { $p => 1, _ => 0 }) };
  v = Some(5); x = 5;
  matches(Some(^x), v) * 8
    + matches(I64 -> Bool, I64 -> Bool) * 4
    + matches(Type, Type) * 2
    + matches((a, b), Tuple(2, I64, Bool))
}
