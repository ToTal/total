(** Desugaring of input syntax to internal syntax. *)

(** [desugar sigma e] converts an expression of type [Input.expr] to type
    [Syntax.expr] by replacing names in [e] with de Bruijn indices. *)
let desugar sigma =
  let rec desugar gamma (e, loc) =
    (match e with
      | Input.Var x ->
	 if Context.mem x sigma then
	   Syntax.Const x
	 else
	   Syntax.Var (Context.index ~loc x gamma)
      | Input.Universe u -> Syntax.Universe u
      | Input.Pi a -> Syntax.Pi (desugar_abstraction gamma a)
      | Input.Lambda a -> Syntax.Lambda (desugar_abstraction gamma a)
      | Input.App (e1, e2) -> Syntax.App (desugar gamma e1, desugar gamma e2)
      | Input.Ann (e1, e2) -> Syntax.Ann (desugar gamma e1, desugar gamma e2)),
    loc
  and desugar_abstraction gamma (x, t, e) =
    let t = desugar gamma t in
    (x, t, desugar (Context.extend gamma (x, t)) e)
  in
    desugar Context.empty_context

