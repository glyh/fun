let type_rhs_to_struct_body (rhs : Ast.type_rhs) : Ast.struct_body =
  match rhs with
  | Adt ctors -> Variants ctors
  | Record fields -> Fields fields

let rec expr (e : Ast.Expr.t) : Desugared_ast.Expr.t =
  match e with
  | Atom a -> Atom a
  | Var id -> Var id
  | Ap (f, x) -> Ap (expr f, expr x)
  | Let { binding = TypeDecl { name; args; rhs }; body } ->
      let struct_val : Desugared_ast.Expr.t =
        StructDef { args; body = type_rhs_to_struct_body rhs; members = [] }
      in
      Let { binding = Value { name; type_ = None; value = struct_val };
            body = Let { binding = Open name; body = expr body } }
  | Let { binding = Value { name; type_; value }; body } ->
      Let { binding = Value { name; type_; value = expr value }; body = expr body }
  | Let { binding = Open name; body } ->
      Let { binding = Open name; body = expr body }
  | Let { binding = Export name; body } ->
      Let { binding = Export name; body = expr body }
  | If { cond; then_; else_ } ->
      If { cond = expr cond; then_ = expr then_; else_ = expr else_ }
  | Lam (param, body) -> Lam (param, expr body)
  | Annotated { inner; typ } -> Annotated { inner = expr inner; typ }
  | Fix inner -> Fix (expr inner)
  | Prod elements -> Prod (Std.Nonempty_list.map expr elements)
  | Match { matched; branches } ->
      Match { matched = expr matched;
              branches = Std.Nonempty_list.map (fun (pat, body) -> (pat, expr body)) branches }
  | RecordConstruct { path; name; fields } ->
      RecordConstruct { path; name;
                        fields = Std.Nonempty_list.map (fun (n, v) -> (n, expr v)) fields }
  | FieldAccess (e, field) -> FieldAccess (expr e, field)
  | StructDef { args; body; members } ->
      StructDef { args; body; members = struct_defs members }
  | Import path -> Import path

and struct_def (sd : Ast.Struct_def.t) : Desugared_ast.Struct_def.t list =
  match sd.binding with
  | TypeDecl { name; args; rhs } ->
      let struct_val : Desugared_ast.Expr.t =
        StructDef { args; body = type_rhs_to_struct_body rhs; members = [] }
      in
      let let_binding : Desugared_ast.Struct_def.t =
        { vis = sd.vis; binding = Value { name; type_ = None; value = struct_val } }
      in
      let open_binding : Desugared_ast.Struct_def.t =
        match sd.vis with
        | Public -> { vis = Private; binding = Export name }
        | Private -> { vis = Private; binding = Open name }
      in
      [ let_binding; open_binding ]
  | Value { name; type_; value } ->
      [ { vis = sd.vis; binding = Value { name; type_; value = expr value } } ]
  | Open name -> [ { vis = sd.vis; binding = Open name } ]
  | Export name -> [ { vis = sd.vis; binding = Export name } ]

and struct_defs (defs : Ast.Struct_def.t list) : Desugared_ast.Struct_def.t list =
  List.concat_map struct_def defs

let binding (b : Ast.Binding.t) : Desugared_ast.Binding.t * Desugared_ast.Binding.t option =
  match b with
  | TypeDecl { name; args; rhs } ->
      let struct_val : Desugared_ast.Expr.t =
        StructDef { args; body = type_rhs_to_struct_body rhs; members = [] }
      in
      (Value { name; type_ = None; value = struct_val }, Some (Open name))
  | Value { name; type_; value } ->
      (Value { name; type_; value = expr value }, None)
  | Open name -> (Open name, None)
  | Export name -> (Export name, None)

let toplevel (bindings : Ast.Binding.t list) : Desugared_ast.Binding.t list =
  List.concat_map (fun b ->
    let main, extra = binding b in
    match extra with
    | None -> [ main ]
    | Some e -> [ main; e ]
  ) bindings
