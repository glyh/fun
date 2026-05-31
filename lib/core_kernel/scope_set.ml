type t = int list

let empty = []
let is_empty = function [] -> true | _ -> false

let union a b =
  let rec go a b =
    match a, b with
    | [], _ -> b
    | _, [] -> a
    | x :: xs, y :: ys ->
      if x < y then x :: go xs b
      else if x > y then y :: go a ys
      else x :: go xs ys
  in
  go a b

let inter a b =
  let rec go a b =
    match a, b with
    | [], _ | _, [] -> []
    | x :: xs, y :: ys ->
      if x < y then go xs b
      else if x > y then go a ys
      else x :: go xs ys
  in
  go a b

let diff a b =
  let rec go a b =
    match a, b with
    | [], _ -> []
    | a, [] -> a
    | x :: xs, y :: ys ->
      if x < y then x :: go xs b
      else if x > y then go a ys
      else go xs ys
  in
  go a b

let subset a b =
  let rec go a b =
    match a, b with
    | [], _ -> true
    | _, [] -> false
    | x :: xs, y :: ys ->
      if x = y then go xs ys
      else if x > y then go a ys
      else false
  in
  go a b

let add s x =
  let rec go s =
    match s with
    | [] -> [x]
    | y :: _ when x < y -> x :: s
    | y :: _ when x = y -> s
    | y :: ys -> y :: go ys
  in
  go s

let remove s x =
  let rec go s =
    match s with
    | [] -> []
    | y :: _ when x < y -> s
    | y :: ys when x = y -> ys
    | y :: ys -> y :: go ys
  in
  go s

let contains s x =
  List.exists (fun y -> y = x) s

let equal a b = a = b

let singleton x = [x]

let of_list l =
  List.sort_uniq compare l

let pp fmt s =
  match s with
  | [] -> Format.fprintf fmt "{}"
  | _ ->
    Format.fprintf fmt "{";
    let rec go = function
      | [] -> Format.fprintf fmt "}"
      | [x] -> Format.fprintf fmt "%d}" x
      | x :: xs -> Format.fprintf fmt "%d," x; go xs
    in
    go s

let size s = List.length s
