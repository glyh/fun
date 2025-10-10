type 'a t = 'a * 'a list

let cons x (hd, rest) = (x, hd :: rest)
let first (hd, _) = hd
