# match tagged payload
{ type Wrapper = W(I64); match (W(41)) { W(x) => x + 1 } }
