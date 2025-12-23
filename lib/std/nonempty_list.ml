type 'a t = 'a * 'a list [@@deriving eq]

let init x l = (x, l)
let uncons = Stdlib.Fun.id
let cons x (hd, rest) = (x, hd :: rest)
let first (hd, _) = hd
let map f (hd, rest) = (f hd, List.map f rest)
let to_list (hd, rest) = hd :: rest
