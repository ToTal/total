open Syntax

let rec is_kind c = function 
  | Universe _, _ -> true
  | Pi (_,_, e),_ -> is_kind c e
  | Subst _ as e, l -> is_kind c (Norm.whnf c (e, l))
  | _, _ -> false

let ctx_from sigma = (sigma, Context.empty_context)

let elab_type_constr sigma x t =
  let ctx = ctx_from sigma in
  let _ = Typing.infer ctx t in
  if not (is_kind ctx t) then
    Error.typing ~loc:(snd t) "expresion @ %t@ is not a kind" (Print.expr ctx t) ;
  Context.add_constr x t sigma

let rec constructs_type x = function
  | Pi (_,_,e),_ -> constructs_type x e
  | App ((Const x', _), _),_ when x = x' -> true
  | Const x',_ when x = x' -> true
  | _ -> false

let positive t x = true 	(* TODO implement this! *)

let validate_constrs (sigma : Context.signature) 
		 (x : Common.name) 
		 (t : Syntax.expr) 
		 (cs : (Common.name * Syntax.expr) list) =
  let elab sigma (c, t) = 
    let ctx = ctx_from sigma in
    if not (constructs_type x t) then
      Error.typing ~loc:(snd t) 
		   "constructor %s does not construct type %s (%t )" 
		   c x (Print.expr ctx t) ;
    let k = Typing.infer ctx t in
    if not (is_kind ctx k)then
      Error.typing ~loc:(snd t)
    		   "constructor %s does not construct a type (%t has type %t)"
    		   c (Print.expr ctx t) (Print.expr ctx k) ;
    if not (positive t x) then
      Error.typing ~loc:(snd t) "constructor %s is not strictly positive." c;
    (* let sigma = Context.add_constr c t sigma in *)
    Context.add_constr c t sigma
  in
  List.fold_left elab sigma cs

let elim sigma d t cs = 
  let nw = Common.nowhere in
  (* telescope of the type constructor *)
  let ty_tel = get_telescope t in let ty_tel_length = List.length ty_tel in
  (* x : D Θ *)
  let vars_for_tel = List.mapi (fun n _ -> nw (Var (ty_tel_length - 1 - n))) ty_tel in
  let d_Theta = List.fold_left (fun e v -> nw(App (e, v))) (nw (Const d)) vars_for_tel in
  let target = (Common.some d, d_Theta) :: ty_tel in




  let elim = List.fold_left (fun a (c, t) -> nw (Pi (c, t, a))) (nw (Universe 0)) target in
  Context.add_constr (d^"-elim") elim sigma
