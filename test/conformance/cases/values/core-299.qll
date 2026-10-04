# brace and expression arms
{
       type Color = Red | Green | Blue;
       pick = fn(c) { match (c) { Red | Blue => { x = 1; x + 1 } Green => 3, } };
       syntax twice { twice $x => $x + $x, twice => 0 };
       pick(Blue) * 10 + (twice 1) + match (Green) { Green => if (True) { 1 } else { 2 }, _ => 0 }
     }
