type 'a t = 'a * 'a list

let init x l = (x, l)
let uncons = Stdlib.Fun.id
let cons x (hd, rest) = (x, hd :: rest)
let first (hd, _) = hd
