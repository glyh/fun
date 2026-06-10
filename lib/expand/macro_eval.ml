let wrap_stx (stx : Syntax.t) : Core.value = Core.VStx (Core.StxExpr stx)

let unwrap_stx (v : Core.value) : Syntax.t option =
  match v with Core.VStx (Core.StxExpr stx) -> Some stx | _ -> None
