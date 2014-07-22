(** Desugaring of input syntax to internal syntax. *)

(** [desugar sigma e] converts an expression of type [Input.expr] to type
    [Syntax.expr] by replacing names in [e] with de Bruijn indices. *)
let desugar sigma =
  let rec desugar gamma (e, loc) =
    let d = desugar gamma in
    (match e with
      | Input.Var x ->
	 if Ctx.mem x sigma then
	   Syntax.Const x
	 else
	   Syntax.Var (Ctx.index ~loc x gamma)
      | Input.Type -> Syntax.Type
      | Input.Pi a -> Syntax.Pi (desugar_abstraction gamma a)
      | Input.Lambda a -> Syntax.Lambda (desugar_abstraction gamma a)
      | Input.App (e1, e2) -> Syntax.App (d e1, d e2)
      | Input.Ann (e1, e2) -> Syntax.Ann (d e1, d e2)
      | Input.HEq (t1, t2, e1, e2) -> Syntax.HEq (d t1, d t2, d e1, d e2)
      | Input.HRefl -> Syntax.HRefl
      | Input.HSubst -> Syntax.HSubst
    ), loc
  and desugar_abstraction gamma (x, t, e) =
    let t = desugar gamma t in
    (x, t, desugar (Ctx.extend gamma (x, t)) e)
  in
    desugar Ctx.empty_context

