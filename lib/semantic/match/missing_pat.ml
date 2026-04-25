type t = Wildcard | Many of (string * t option) Std.Nonempty_list.t

let rec equal_opt a b =
  match (a, b) with
  | None, None -> true
  | Some a, Some b -> equal a b
  | _ -> false

and equal_entry (n1, p1) (n2, p2) = String.equal n1 n2 && equal_opt p1 p2

and equal a b =
  match (a, b) with
  | Wildcard, Wildcard -> true
  | Many xs, Many ys ->
      let xs = Std.Nonempty_list.to_list xs in
      let ys = Std.Nonempty_list.to_list ys in
      List.length xs = List.length ys
      && List.for_all (fun x -> List.exists (equal_entry x) ys) xs
  | _ -> false

let rec pp = function
  | Wildcard -> "_"
  | Many ctors ->
      ctors |> Std.Nonempty_list.to_list
      |> List.map (fun (name, inner) ->
             match inner with
             | None -> name
             | Some p -> name ^ " (" ^ pp p ^ ")")
      |> String.concat " | "
