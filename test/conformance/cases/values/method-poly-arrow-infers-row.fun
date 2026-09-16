# a method written ~> T infers its row from what its body performs
{ effect Log = sig { write : I64 -> I64 }; S = struct { v : I64; pub method run() ~> I64 { perform Log.write(self.v) } }; match (S.run(S{v = 5})) { r => 0, effect Log.write n => n } }
