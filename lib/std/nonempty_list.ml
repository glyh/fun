open Core

type +'a t = 'a * 'a list [@@deriving eq, hash]

let init x l = (x, l)
let uncons = Stdlib.Fun.id
let cons x (hd, rest) = (x, hd :: rest)
let first (hd, _) = hd
let map f (hd, rest) = (f hd, List.map ~f rest)
let to_list (hd, rest) = hd :: rest

let iter f (hd, rest) =
  f hd;
  List.iter ~f rest

let iteri f (hd, rest) =
  f 0 hd;
  List.iteri ~f:(fun i x -> f (i + 1) x) rest

let zip_exn (hd1, rest1) (hd2, rest2) = ((hd1, hd2), List.zip_exn rest1 rest2)
