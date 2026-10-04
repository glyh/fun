# the caller's type is not the macro's
{
       type Tmp = Yes | No;
       macro with_tmp(e) { quote({ rec Tmp = enum { A, B }; open Tmp; $e }) };
       with_tmp({ v : Tmp = Yes; match (v) { Yes => 1, No => 0 } })
     }
