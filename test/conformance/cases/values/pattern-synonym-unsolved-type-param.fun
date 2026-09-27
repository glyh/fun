# a use whose scrutinee type is a bare meta (inside an unannotated lambda): nothing
# determines the synonym's type parameter, so its scope end reports it rather than
# leaving the use stuck without saying so
{ M = module { pub pattern Two(a, b) = (a, b) };
  f = fn(v) { match (v) { M.Two(x, y) => x } };
  9 }
