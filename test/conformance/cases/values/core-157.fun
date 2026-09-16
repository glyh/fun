# record pattern literal dispatch
{ Flag = struct { flag: Bool; value: I64; }; match (Flag{flag = False; value = 3}) { Flag {flag = True; value} => value, Flag {flag = False; value} => value + 1 } }
