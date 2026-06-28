type 'a t = {
  run : Enforest_util.env -> Raw_syntax.t list -> ('a * Raw_syntax.t list) option;
  name : string;
}

let step t = t.run

let pure x = { run = (fun _env ts -> Some (x, ts)); name = "pure" }

let map spec f = {
  run = (fun env ts -> match spec.run env ts with Some (v, r) -> Some (f v, r) | None -> None);
  name = spec.name
}

let bind spec f = {
  run = (fun env ts ->
    match spec.run env ts with
    | Some (v, rest) -> (f v).run env rest
    | None -> None);
  name = spec.name
}

let seq a b = {
  run = (fun env ts ->
    match a.run env ts with
    | Some (va, rest) -> (match b.run env rest with Some (vb, rest2) -> Some ((va, vb), rest2) | None -> None)
    | None -> None);
  name = Printf.sprintf "(%s ; %s)" a.name b.name
}

let seq3 a b c = map (seq (seq a b) c) (fun ((va, vb), vc) -> (va, vb, vc))
let seq4 a b c d = map (seq (seq3 a b c) d) (fun ((va, vb, vc), vd) -> (va, vb, vc, vd))
let seq5 a b c d e = map (seq (seq4 a b c d) e) (fun ((va, vb, vc, vd), ve) -> (va, vb, vc, vd, ve))

let alt specs = {
  run = (fun env ts ->
    let rec go = function
      | [] -> None
      | s :: rest -> match s.run env ts with Some r -> Some r | None -> go rest
    in go specs);
  name = "alt"
}

let opt spec = alt [ map spec (fun x -> Some x); pure None ]

let many spec = {
  run = (fun env ts ->
    let rec go acc ts =
      match spec.run env ts with
      | Some (v, rest) -> go (v :: acc) rest
      | None -> Some (List.rev acc, ts)
    in go [] ts);
  name = Printf.sprintf "many(%s)" spec.name
}

let eof = {
  run = (fun _env -> function [] -> Some ((), []) | _ -> None);
  name = "eof"
}

let token pred mk = {
  run = (fun _env -> function
    | t :: rest when pred t -> Some (mk t, rest)
    | _ -> None);
  name = "token"
}

let punct kind = {
  run = (fun _env -> function
    | t :: rest when Enforest_util.token_kind kind t -> Some ((), rest)
    | _ -> None);
  name = Enforest_util.punct_name kind |> Option.value ~default:"?"
}

let ident = {
  run = (fun _env -> function
    | { datum = Token { kind = Ident n; _ }; span = span } :: rest ->
        Some (({ Syntax.name = n; span; scope = Scope_set.empty }, span), rest)
    | _ -> None);
  name = "ident"
}

let group delimiter item_spec = {
  run = (fun env -> function
    | { datum = Group (delim, items, span); _ } :: rest when delim = delimiter ->
        let items = Enforest_util.drop_separators items in
        (match item_spec.run env items with
         | Some (value, []) -> Some ((value, span), rest)
         | _ -> None)
    | _ -> None);
  name = Printf.sprintf "group(%s)" item_spec.name
}

let drop_sep spec = {
  run = (fun env ts -> spec.run env (Enforest_util.drop_separators ts));
  name = spec.name
}

let parse spec env ts = (drop_sep spec).run env ts

let recover spec = {
  run = (fun env ts ->
    match spec.run env (Enforest_util.drop_separators ts) with
    | Some result -> Some result
    | None ->
        let span = match ts with
          | t :: _ -> t.span
          | [] -> Source_span.synthetic
        in
        Enforest_util.push_error env span
          (Parse_error.Unexpected { expected = spec.name; got = "?" });
        None);
  name = spec.name
}

let str_ident = {
  run = (fun _env -> function
    | { datum = Token { kind = Ident n; _ }; span } :: rest ->
        Some ((n, span), rest)
    | _ -> None);
  name = "ident"
}

let to_option spec env tokens =
  match parse spec env tokens with
  | Some (v, []) -> Some v
  | _ -> None
