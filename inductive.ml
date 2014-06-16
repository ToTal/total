open Syntax
open Su

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
  Ctx.add_constr x t 0 sigma 	(* type constructor is number 0 *)

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
  let elab (sigma, n) (c, t) = 
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
    (Ctx.add_constr c t n sigma, n+1)
  in
  fst(List.fold_left elab (sigma, 0) (List.rev cs)) (* TODO use fold_right? *)

let nw = Common.nowhere
(** Computes the induction hypothesis *)
let motive_ty sigma d t = 
  let ctx = ctx_from sigma in
  Print.debug "Building motive for type %s : %t" d (Print.expr ctx t);
  let params,_ = get_telescope ctx t in
  let d' = join_head_spine (nw (Const d)) (List.map (fun (x, _) -> var x) params) in

  let p = set_telescope ctx ((Common.none_with "D", d')::params) (nw (Universe 0)) (fun x t e -> nw(Pi(x, t, e))) in
  Print.debug "Motive for %s is P : %t" d (Print.expr ctx p) ;
  p

let method_ty sigma d t c ct p_nm = 
  let rec constructor_params_for p = function
    | App (e1, e2), l -> App(constructor_params_for p e1, e2), l
    | Const _, l -> (fst p), l
    | _ -> Error.violation "This is not happening"
  in
  let ctx = ctx_from sigma in
  Print.debug "Computing method : %s" c ;
  (* The term that contains P *)
  let p = var p_nm in
  
  (* All the constructor's parameters *)
  let constr_tel, constr = get_telescope ctx ct in
  Print.debug "constr_tel length = %d"  (List.length constr_tel) ;
  Print.debug "constr_tel = [%t]" (Print.sequence ~sep:" ;" (fun (_,e) -> Print.expr ctx e) constr_tel) ;

  (* The parameters that represent a recursive call *)
  let recs = List.filter (fun (x, t) -> produces_constr ctx d t) constr_tel in
  (* let recs = List.filter (fun (x, t) -> is_constr ctx d t) constr_tel in *)

  Print.debug "t = %t" (Print.expr ctx t) ;
  Print.debug "d = %s" d ;
  Print.debug "recs = [%t]" (Print.sequence ~sep:" ;" (fun (_,e) -> Print.expr ctx e) recs) ;

  let hyps = List.map 
	       (fun (x, t) -> 
		Common.none_with "r", 
		let t_tel, t_body = get_telescope ctx t in
		Print.debug "t_tel = %t" (Print.tele ctx t_tel) ;
		Print.debug "t_body = %t" (Print.expr ctx t_body) ;
		let r_app = join_head_spine (var x) (List.map (fun (y,_) -> var y) t_tel) in
		set_telescope ctx 
			      t_tel
			      (nw(App (constructor_params_for p t_body, r_app)))
			      (fun x t e -> nw(Pi(x,t,e))))
	       recs
  in
  let final_tel = hyps @ (List.rev constr_tel) in (* I'm confused about this List.rev *)

  (* p with argments up to D applied *) 
  let p' = constructor_params_for p constr in
  Print.debug "What I want to know is: %t" (Print.expr ctx constr) ;

  let result = nw (App (p', join_head_spine (nw (Const c)) (List.map (fun (x, _) -> var x) constr_tel))) in

  let m = set_telescope ctx final_tel result (fun v t e -> nw (Pi (v, t, e))) in
  Print.debug "For %s method: %t" c (Print.expr ctx m) ;
  m

let elim sigma d t cs = 
  let ctx = ctx_from sigma in
  Print.debug "Computing eliminator for %s" d ;
  let targets, _ = get_telescope ctx t in
  let x = Common.none_with "x" in

  let p_nm = Common.none_with "P" in
  let p = motive_ty sigma d t in

  let ms = List.map
  	     (fun (c, ct) ->
  	      Common.none_with "m", method_ty sigma d t c ct p_nm)
  	     cs
  in

  let x_dest = join_head_spine (nw (Const d)) (List.map (fun (x, _) -> var x) targets) in

  let final_tel = ms @ ((p_nm, p) ::(x, x_dest) :: targets) in

  let result = join_head_spine (var p_nm) (List.map (fun (x,_) -> var x) (List.rev ((x, x_dest)::targets))) in

  Print.debug "Eliminator telescope length: %d" (List.length final_tel) ;
  Print.debug "Eliminator telescope: %t" (Print.sequence ~sep:" ;" (fun (_,e) -> Print.expr ctx e) final_tel) ;
  Print.debug "result = %t" (Print.expr ctx result) ;

  let elim_ty = set_telescope ctx final_tel result (fun v t e -> nw (Pi (v, t, e))) in

  let kind = Typing.infer ctx elim_ty in
  if not (is_kind ctx (Norm.whnf ctx kind)) then
    Error.violation (* ~loc:(snd elim_ty)  *)
      "expresion @ %t@  in eliminator is not a kind @ %t@ (inductive.ml)"
      (Print.expr ctx elim_ty) (Print.expr ctx kind);
  
  Ctx.add_elim (d^"-elim") elim_ty d sigma


