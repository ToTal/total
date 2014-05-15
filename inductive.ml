open Syntax

let rec is_kind = function 
  | Universe _, _ -> true
  | Pi (_,_, e),_ -> is_kind e
  | _, _ -> false

let ctx_from sigma = (sigma, Context.empty_context)

let elab_type_constr sigma x t =
  let ctx = ctx_from sigma in
  let _ = Typing.infer ctx t in
  if not (is_kind t) then
    Error.typing ~loc:(snd t) "expresion @ %t@ is not a kind" (Print.expr ctx t) ;
  Context.add_constr x t sigma

let rec constructs_type x = function
  | Pi (_,_,e),_ -> constructs_type x e
  | App ((Const x', _), _),_ when x = x' -> true
  | Const x',_ when x = x' -> true
  | _ -> false

let positive t x = true 	(* TODO implement this! *)

let elab_constrs (sigma : Context.signature) 
		 (x : Common.variable) 
		 (t : Syntax.expr) 
		 (cs : (Common.variable * Syntax.expr) list) =
  let elab sigma (c, t) = 
    let ctx = ctx_from sigma in
    if not (constructs_type x t) then
    Error.typing ~loc:(snd t) "constructor %s does not construct type %s (%t )" c x  (Print.expr ctx t) ;
    if not (positive t x) then
    Error.typing ~loc:(snd t) "constructor %s is not strictly positive." c;
    Context.add_constr c t sigma 
  in
  List.fold_left elab sigma cs



