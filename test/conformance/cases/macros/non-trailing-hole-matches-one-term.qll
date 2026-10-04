# a non-trailing hole matches exactly one term, so x < n does not reach then
{
  syntax choose {
    choose $cond then $branch else $fallback => if ($cond) { $branch } else { $fallback }
  };
  x = 1;
  n = 2;
  a = 5;
  choose x < n then a else 0
}
