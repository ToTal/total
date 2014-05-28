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
  if not (is_kind ctx (Norm.whnf ctx t)) then
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
  let ctx = ctx_from sigma in
  Print.debug "Building motive for type %s : %t" d (Print.expr ctx t);
  let params,_ = get_telescope t in
  let d' = set_telescope params (nw (Const d)) (fun x _ e -> nw (App (e, var x))) in
  let p = List.fold_left 
	    (fun v (x, t) -> nw(Pi (x, t, var_to_db x v))) 
	    (nw (Universe 0)) 
	    ((Common.none_with "D", d')::params)
  in
  Print.debug "Motive for %s is P : %t" d (Print.expr ctx p) ;
  p

let method_ty sigma d t c ct p_nm = 
  let rec constructor_params_for p = function
    | App (e1, e2), l -> App(constructor_params_for p e1, e2), l
    | Const _, l -> (fst p), l
    | _ -> Error.violation "Pum"
  in

  let ctx = ctx_from sigma in
  Print.debug "Computing method : %s" c ;
  (* The term that contains P *)
  let p = var p_nm in
  
  (* All the constructor's parameters *)
  let constr_tel, constr = get_telescope ct in
  Print.debug "constr_tel length = %d"  (List.length constr_tel) ;
  Print.debug "constr_tel = %t" (Print.tele ctx constr_tel) ;

  let rec is_constr d = function 
    | Const c, l -> d = c
    | App (e, _), l -> is_constr d e
    | _, _ -> false
  in

  (* The parameters that represent a recursive call *)
  let recs = List.filter (fun (x, t) -> is_constr d t) constr_tel in

  Print.debug "t = %t" (Print.expr ctx t) ;
  Print.debug "d = %s" d ;
  Print.debug "len(recs) = %d" (List.length recs) ;

  let hyps = List.map 
	       (fun (x, t) -> 
		Common.none_with "r", 
		nw 
		  (App (constructor_params_for p t, var x))) (* HERE *)
	       recs
  in
  let final_tel = hyps @ (List.rev constr_tel) in (* I'm confused about this List.rev *)

  (* p with argments up to D applied *) 
  let p' = constructor_params_for p constr in
  Print.debug "What I want to know is: %t" (Print.expr ctx constr) ;

  let result = nw (App (p', List.fold_left (fun e (n, _) -> nw(App(e, nw(Free n)))) (nw (Const c)) constr_tel)) in

  let m = set_telescope final_tel result (fun v t e -> nw (Pi (v, t, e))) in
  Print.debug "For %s method: %t" c (Print.expr ctx m) ;
  m

let elim sigma d t cs =
  let ctx = ctx_from sigma in
  Print.debug "Computing eliminator for %s" d ;
  let targets, _ = get_telescope t in
  let x = Common.none_with "x" in

  let p_nm = Common.none_with "P" in
  let p = motive_ty sigma d t in

  let ms = List.map
	     (fun (c, ct) ->
	      Common.none_with "m", method_ty sigma d t c ct p_nm)
	     cs
  in

  let x_dest = set_telescope targets (nw (Const d)) (fun v _ e -> nw (App(e, var v))) in

  let final_tel = ms @ ((p_nm, p) ::(x, x_dest) :: targets) in

  let result = set_telescope (List.rev ((x, x_dest)::targets)) (var p_nm) (fun v _ e -> nw(App(e, var v))) in

  Print.debug "Eliminator telescope length: %d" (List.length final_tel) ;
  Print.debug "Eliminator telescope: %t" (Print.tele ctx final_tel);
  Print.debug "result = %t" (Print.expr ctx result) ;

  let elim_ty = set_telescope final_tel result (fun v t e -> nw (Pi (v, t, e))) in

   let kind = Typing.infer ctx elim_ty in
   if not (is_kind ctx (Norm.whnf ctx kind)) then
     Error.violation ~loc:(snd elim_ty) 
		     "expresion @ %t@  in eliminator is not a kind @ %t@ " 
		     (Print.expr ctx elim_ty) (Print.expr ctx kind);

  Ctx.add_constr (d^"-elim") elim_ty sigma


