open Syntax

let rec is_kind c = function 
  | Universe _, _ -> true
  | Pi (_,_, e),_ -> is_kind c e
  | Subst _ as e, l -> is_kind c (Norm.whnf c (e, l))
  | _, _ -> false

let ctx_from sigma = (sigma, Ctx.empty_context)

let elab_type_constr sigma x t =
  let ctx = ctx_from sigma in
  let _ = Typing.infer ctx t in
  if not (is_kind ctx t) then
    Error.typing ~loc:(snd t) "expresion @ %t@ is not a kind" (Print.expr ctx t) ;
  Ctx.add_constr x t sigma

let rec constructs_type x = function
  | Pi (_,_,e),_ -> constructs_type x e
  | App ((Const x', _), _),_ when x = x' -> true
  | Const x',_ when x = x' -> true
  | _ -> false

let positive t x = true 	(* TODO implement this! *)

let validate_constrs (sigma : Ctx.signature) 
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
    Ctx.add_constr c t sigma
  in
  List.fold_left elab sigma cs

let nw = Common.nowhere
(** Computes the induction hypothesis *)
let motive_ty sigma d t = 
  let params,_ = get_telescope t in let param_len = List.length params in
  let vars = List.mapi (fun n _ -> nw (Var (param_len - 1 - n))) params in
  let d = List.fold_left (fun e v -> nw (App (e, v))) (nw (Const d)) vars in
  List.fold_left 
    (fun v (x, t) -> nw(Pi (x, t, v))) 
    (nw (Universe 0)) 
    ((Common.none_with "D", d)::params)

let method_ty sigma d t c p_idx = 
  (* All the constructor's parameters *)
  let constr_tel,_ = get_telescope c in
  let constr_tel_shift = List.length constr_tel in

  (* The parameters that represent a recursive call *)
  let recs = List.filter (fun (x, t) -> false) constr_tel in

  let rec_call p_idx' = List.mapi 
		     (fun n (x, t) -> 
		      let p = nw (Var (p_idx' + n)) in
		      (Common.none_with "r", p))
		     recs 
  in
  let rec_call_shift = List.length recs in

  let final_p p_idx' = 
    nw (Var p_idx') in		(* TODO apply it to \vec s *)

  let final_tel = constr_tel @ (rec_call (constr_tel_shift + p_idx)) in

  List.fold_left 
    (fun v (x, t) -> nw (Pi (Common.none, t, v)))
    (final_p (p_idx + constr_tel_shift + rec_call_shift)) final_tel


(* let elim sigma d t cs =  *)
(*   let p = motive_ty sigma d t in *)
(*   let ms = List.rev (List.mapi *)
(* 		       (fun n (c, ct) -> *)
(* 			Common.none_with "m", method_ty sigma d t ct n) *)
(* 		       cs) *)
(*   in *)

(*   let final_tel = ms @ ((Common.none_with "P", p) :: []) in *)

(*   let result = nw (Universe 42) in *)
(*   let elim_ty = List.fold_left *)
(*     (fun v (x, t) -> nw (Pi (Common.none, t, v))) *)
(*     result  *)
(*     final_tel *)
(*   in *)

(*   Ctx.add_constr (d^"-elim") elim_ty sigma *)


let elim sigma d t cs = Ctx.add_constr (d^"-elim") (nw (Universe 1729)) sigma 
